-module(listen_srv).

-export([start_link/3]).
-export([listen/3]).

start_link(Port, TCPOptions, Parent) ->
    spawn_link(listen_srv, listen, [Port, TCPOptions, Parent]).

listen(Port, TCPOptions, Parent) ->
    Result = gen_tcp:listen(Port, TCPOptions),
    {ok, LSocket} = Result,
    accept_loop(LSocket, Parent).

accept_loop(LSocket, Parent) ->
    case gen_tcp:accept(LSocket) of
        {ok, Socket} ->
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
