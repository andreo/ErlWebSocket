
-module(websocket_connection_fsm_test).

-include_lib("eunit/include/eunit.hrl").

make_response_test() ->
    Headers = [{http_header,0,<<"Sec-Websocket-Key2">>,undefined,
                <<"4 131ga0 3   O%y54 $Q    3Um0Ee">>},
               {http_header,0,<<"Sec-Websocket-Key1">>,undefined,
                <<"1    2 308   9 8 x  128">>},
               {http_header,0,<<"Origin">>,undefined,<<"null">>},
               {http_header,14,'Host',undefined,<<"localhost:8080">>},
               {http_header,2,'Connection',undefined,<<"Upgrade">>},
               {http_header,6,'Upgrade',undefined,<<"WebSocket">>},
               {http_request,'GET',{abs_path,<<"/">>},{1,1}}],

    InputKey = <<26,218,112,46,238,243,114,105>>,
    R = websocket_connection_fsm:make_response(Headers, InputKey),
    R = <<"HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
         "Upgrade: WebSocket\r\n"
         "Connection: Upgrade\r\n"
         "Sec-WebSocket-Origin: null\r\n"
         "Sec-WebSocket-Location: ws://localhost:8080/\r\n"
         "\r\n",
         194,146,87,142,179,211,119,185,229,21,180,202,229,121,141,213>>.
