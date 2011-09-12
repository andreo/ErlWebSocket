
-module(handshake_test).

-include_lib("eunit/include/eunit.hrl").

is_digit_test() ->
    false = handshake:is_digit($0-1),
    true = handshake:is_digit($0),
    true = handshake:is_digit($5),
    true = handshake:is_digit($9),
    false = handshake:is_digit($9+1).
    
digits_test() ->
    <<"1234">> = handshake:digits(<<"a1s2d3f4">>),
    <<"">> = handshake:digits(<<"aaa">>).
    
extract_number_test() ->
    %% empty list is not supported
    %% ??? = handshake:extract_number(<<"">>),
    2 = handshake:extract_number(<<"2">>),
    12345 = handshake:extract_number(<<"a1s2d3f4g5">>),
    1868545188 = handshake:extract_number(<<"18x 6]8vM;54 *(5:  {   U1]8  z [  8">>),
    1733470270 = handshake:extract_number(<<"1_ tx7X d  <  nw  334J702) 7]o}` 0">>),
    ok.

num_spaces_test() ->
    0 = handshake:num_spaces(<<"">>),
    0 = handshake:num_spaces(<<"a">>),
    1 = handshake:num_spaces(<<" www">>),
    5 = handshake:num_spaces(<<" www    ">>),
    12 = handshake:num_spaces(<<"18x 6]8vM;54 *(5:  {   U1]8  z [  8">>),
    10 = handshake:num_spaces(<<"1_ tx7X d  <  nw  334J702) 7]o}` 0">>),
    ok.
