
-module(connection_sup).
-beheviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) when is_integer(Port) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Port).

init(Port) when is_integer(Port) ->
    {ok, {{simple_one_for_one, 0, 1},
          [{connection, {websocket_connection_fsm, start_link, []},
            temporary, 2000, worker, [websocket_connection_fsm]}]}}.
