%% -*- mode: erlang; -*-

{application, websocket,
 [{description, "Simple Websocket Server" },
  {vsn, "1.0" },
  {modules, [
             connection_sup,
             handshake,
             handshake_test,
             listen_manager_srv,
             listen_srv,
             utils,
             utils_test,
             websocket_app,
             websocket_connection_fsm,
             websocket_connection_fsm_test,
             websocket_srv_test,
             websocket_sup
             ]},
  {registered,[
               listen_manager_srv,
               websocket_connection_fsm,
               connection_sup,
               websocket_sup
              ]},
  {applications, [kernel,stdlib]},
  {mod, {websocket_app,8080}}
  %% {start_phases, []}
 ]}.


