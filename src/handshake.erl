
%% http://www.whatwg.org/specs/web-socket-protocol/

-module(handshake).

-export([
         digits/1,
         is_digit/1,
         extract_number/1,
         num_spaces/1,
         magic_number/1,
         magic_number/3
        ]).

is_digit(X) ->
    ($0 =< X) and (X =< $9).

digits(Chars) ->
    << <<X>> || <<X>> <= Chars, is_digit(X) >>.

extract_number(Chars) ->
    Digits = digits(Chars),
    List = binary_to_list(Digits),
    list_to_integer(List).

num_spaces(Chars) ->
    size(<< <<X>> || <<X>> <= Chars, X == $\s>>).

magic_number(Bin) ->
    N = extract_number(Bin) div num_spaces(Bin),
    <<N:32/big>>.

magic_number(Bin1, Bin2, Bin3) ->
    N1 = magic_number(Bin1),
    N2 = magic_number(Bin2),
    erlang:md5(<<N1/binary, N2/binary, Bin3/binary>>).
