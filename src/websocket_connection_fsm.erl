%%%-------------------------------------------------------------------
%%% File    : websocket_srv.erl
%%% Author  : andreo <andreo@andreo-laptop>
%%% Description : websocket 
%%%
%%% Created :  8 Sep 2011 by andreo <andreo@andreo-laptop>
%%%-------------------------------------------------------------------
-module(websocket_connection_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, state_name/2, state_name/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
        make_response/2]).

-record(state_recv_http_header, {socket, headers=[]}).
-record(state_recv_input_key, {socket, headers=[], input_key = <<>>}).
-record(state_recv_message, {socket, rest = <<>>}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> ok,Pid} | ignore | {error,Error}
%% Description:Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this function
%% does not return until Module:init/1 has returned.  
%%--------------------------------------------------------------------
start_link(Socket) ->
    %% {local, ?MODULE}
    gen_fsm:start_link(?MODULE, [Socket], [{debug, [log, trace]}]).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, StateName, State} |
%%                         {ok, StateName, State, Timeout} |
%%                         ignore                              |
%%                         {stop, StopReason}                   
%% Description:Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/3,4, this function is called by the new process to 
%% initialize. 
%%--------------------------------------------------------------------
init([Socket]) ->
    %% io:format("init ~p~n", [Socket]),
    {ok, recv_http_header, #state_recv_http_header{socket=Socket}}.

%%--------------------------------------------------------------------
%% Function: 
%% state_name(Event, State) -> {next_state, NextStateName, NextState}|
%%                             {next_state, NextStateName, 
%%                                NextState, Timeout} |
%%                             {stop, Reason, NewState}
%% Description:There should be one instance of this function for each possible
%% state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_event/2, the instance of this function with the same name as
%% the current state name StateName is called to handle the event. It is also 
%% called if a timeout occurs. 
%%--------------------------------------------------------------------
state_name(_Event, State) ->
    {next_state, recv_http_header, State}.

%%--------------------------------------------------------------------
%% Function:
%% state_name(Event, From, State) -> {next_state, NextStateName, NextState} |
%%                                   {next_state, NextStateName, 
%%                                     NextState, Timeout} |
%%                                   {reply, Reply, NextStateName, NextState}|
%%                                   {reply, Reply, NextStateName, 
%%                                    NextState, Timeout} |
%%                                   {stop, Reason, NewState}|
%%                                   {stop, Reason, Reply, NewState}
%% Description: There should be one instance of this function for each
%% possible state name. Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_event/2,3, the instance of this function with the same
%% name as the current state name StateName is called to handle the event.
%%--------------------------------------------------------------------
state_name(_Event, _From, State) ->
    Reply = ok,
    {reply, Reply, state_name, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_event(Event, StateName, State) -> {next_state, NextStateName, 
%%						  NextState} |
%%                                          {next_state, NextStateName, 
%%					          NextState, Timeout} |
%%                                          {stop, Reason, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:send_all_state_event/2, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_sync_event(Event, From, StateName, 
%%                   State) -> {next_state, NextStateName, NextState} |
%%                             {next_state, NextStateName, NextState, 
%%                              Timeout} |
%%                             {reply, Reply, NextStateName, NextState}|
%%                             {reply, Reply, NextStateName, NextState, 
%%                              Timeout} |
%%                             {stop, Reason, NewState} |
%%                             {stop, Reason, Reply, NewState}
%% Description: Whenever a gen_fsm receives an event sent using
%% gen_fsm:sync_send_all_state_event/2,3, this function is called to handle
%% the event.
%%--------------------------------------------------------------------
handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

%%--------------------------------------------------------------------
%% Function: 
%% handle_info(Info,StateName,State)-> {next_state, NextStateName, NextState}|
%%                                     {next_state, NextStateName, NextState, 
%%                                       Timeout} |
%%                                     {stop, Reason, NewState}
%% Description: This function is called by a gen_fsm when it receives any
%% other message than a synchronous or asynchronous event
%% (or a system message).
%%--------------------------------------------------------------------
continue_recv_http_header(State=#state_recv_http_header{headers=Headers}, NewHeader) ->
    {next_state,
     recv_http_header,
     State#state_recv_http_header{headers=[NewHeader|Headers]}}.

handle_info(Message,
            recv_http_header,
            State=#state_recv_http_header{socket=Socket, headers=Headers}) ->
    %% io:format("handle_info: ~p~n", [Message]),
    case Message of
        {http, Socket, Error={http_error, _Reason}} ->
            {stop, Error, State};
        {http, Socket, Request={http_request,_,_,_}} ->
            continue_recv_http_header(State, Request);
        {http, Socket, Header={http_header,_,_Key,_,_Value}} ->
            continue_recv_http_header(State, Header);
        {http, Socket, http_eoh} ->
            ok = inet:setopts(Socket, [{packet, raw}]),
            {next_state, recv_input_key, #state_recv_input_key{socket=Socket,
                                                               headers=Headers
                                                              }};
        Other ->
            {stop, Other, State}
    end;
handle_info(Message,
            recv_input_key,
            State=#state_recv_input_key{socket=Socket,
                                      headers=Headers,
                                      input_key=OldData}) ->
    case Message of
        {tcp, Socket, NewData} ->
            case utils:accum_n_bytes(8, OldData, NewData) of
                {InputKey, Rest} ->
                    ok = do_handshake(Socket, Headers, InputKey),
                    {next_state, recv_message, #state_recv_message{socket=Socket,
                                                                   rest=Rest}};
                Incomplete ->
                    {next_state, recv_input_key, State#state_recv_input_key{
                                                   input_key=Incomplete}}
            end;
        {tcp_closed, Socket} ->
            {stop, normal, State};
        Other ->
            {stop, Other, State}
    end;
handle_info(Message,
            recv_message,
            State=#state_recv_message{socket=Socket,
                                      rest=Rest}) ->
    case Message of
        {tcp, Socket, NewData} ->
            Data = <<Rest/binary, NewData/binary>>,
            case utils:extract_message(Data) of
                {ok, TextMessage, NewRest} ->
                    ok = gen_tcp:send(Socket, <<0:8, TextMessage/binary, 255:8>>),
                    {next_state, recv_message, #state_recv_message{socket=Socket,
                                                                   rest=NewRest}};
                {more, Data} ->
                    {next_state, recv_message, #state_recv_message{socket=Socket,
                                                                   rest=Data}}
            end;
        {tcp_closed, Socket} ->
            {stop, normal, State};
        Other ->
            {stop, Other, State}
    end;
handle_info(Any, 
            AnyState,
            State)->
    {stop, {unknown, Any, AnyState}, State}.
    

%%--------------------------------------------------------------------
%% Function: terminate(Reason, StateName, State) -> void()
%% Description:This function is called by a gen_fsm when it is about
%% to terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_fsm terminates with
%% Reason. The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _StateName, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Function:
%% code_change(OldVsn, StateName, State, Extra) -> {ok, StateName, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
make_response(Headers, InputKey) ->
    {http_header,_,<<"Sec-Websocket-Key1">>,_, Key1} = lists:keyfind(<<"Sec-Websocket-Key1">>, 3, Headers),
    {http_header,_,<<"Sec-Websocket-Key2">>,_, Key2} = lists:keyfind(<<"Sec-Websocket-Key2">>, 3, Headers),
    {http_header,_,'Host',_,Host} = lists:keyfind('Host', 3, Headers),
    {http_request,_,{abs_path,Path},_} = lists:keyfind(http_request, 1, Headers),
    Magic = handshake:magic_number(Key1, Key2, InputKey),

    Response = <<"HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
                "Upgrade: WebSocket\r\n"
                "Connection: Upgrade\r\n"
                "Sec-WebSocket-Origin: null\r\n"
                "Sec-WebSocket-Location: ws://", Host/binary, Path/binary, "\r\n"
                "\r\n", Magic/binary>>,
    Response.

do_handshake(Socket, Headers, InputKey) ->
    Response = make_response(Headers, InputKey),
    ok = gen_tcp:send(Socket, Response),
    %% ok = gen_tcp:send(Socket, <<0:8, "hello\n", 255:8>>),
    ok.
