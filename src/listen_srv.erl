-module(listen_srv).

-export([start_link/3]).
-export([listen/3]).

start_link(Port, TCPOptions, Parent) ->
    spawn_link(listen_srv, listen, [Port, TCPOptions, Parent]).

listen(Port, TCPOptions, Parent) ->
    %% io:format("listen: ~p~n", [sys:get_status(self())]),
    %% io:format("~p:listen: self:~p port=~p~n", [?MODULE, self(), Port]),
    Result = gen_tcp:listen(Port, TCPOptions),
    %% io:format("~p:listen: socket=~p~n", [?MODULE, Result]),
    {ok, LSocket} = Result,
    accept_loop(LSocket, Parent).

accept_loop(LSocket, Parent) ->
    %% io:format("accept_loop: ~p~n", [LSocket]),
    %% helper:random_error(),
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
            io:format("*DBG* ~p new connection ~p~n", [?MODULE, Socket]),
            ok = gen_tcp:controlling_process(Socket, Parent),
            Parent ! {accept_socket, self(), Socket},
            receive
                Data ->
                    io:format("accept_loop: ~p~n", [Data]),
                    {unknown, Data}
            after 1 ->
                    accept_loop(LSocket, Parent)
            end;
        {error, Reason}=Error ->
            io:fwrite("Socket accept error: ~s~n", [Reason]),
            Error
    end.
