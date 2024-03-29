%%%-------------------------------------------------------------------
%%% File    : listen_manager_srv.erl
%%% Author  : andreo <andreo@andreo-laptop>
%%% Description : 
%%%
%%% Created : 10 Sep 2011 by andreo <andreo@andreo-laptop>
%%%-------------------------------------------------------------------
-module(listen_manager_srv).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {lpid, port}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], [{debug, [log, trace]}]).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Port]) ->
    process_flag(trap_exit, true),
    TCPOptions = [binary,
                  {reuseaddr, true},
                  {active, false}
                  %% {packet, http_bin}
                 ],
    LPid = listen_srv:start_link(Port, TCPOptions, self()),
    {ok, #state{lpid=LPid, port=Port}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(Message, State) ->
    {stop, {unknown, Message}, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({accept_socket, From, Socket}=_Message, State=#state{lpid=From}) ->
    {ok, Pid} = supervisor:start_child(connection_sup, [Socket]),
    ok = gen_tcp:controlling_process(Socket, Pid),
    ok = inet:setopts(Socket, [{packet, http_bin}, {active, true}]),
    {noreply, State};
handle_info({'EXIT',  LPid, _LReason}=Reason, State=#state{lpid=LPid}) ->
    {stop, Reason, State};
handle_info(Message, State) ->
    {stop, {unknown, Message}, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
