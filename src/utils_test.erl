
-module(utils_test).

-include_lib("eunit/include/eunit.hrl").

accum_n_bytes__empty_state__test() ->
    <<"a">> = utils:accum_n_bytes(3, <<>>, <<"a">>),
    {<<"abc">>, <<>>} = utils:accum_n_bytes(3, <<>>, <<"abc">>),
    {<<"abc">>, <<"d">>} = utils:accum_n_bytes(3, <<>>, <<"abcd">>).

accum_n_bytes__incomple_state__test() ->
    <<"ab">> = utils:accum_n_bytes(3, <<"ab">>, <<>>),
    {<<"abc">>, <<>>} = utils:accum_n_bytes(3, <<"ab">>, <<"c">>),
    {<<"abc">>, <<"d">>} = utils:accum_n_bytes(3, <<"ab">>, <<"cd">>).

accum_n_bytes__complete_state__test() ->
    {<<"abc">>, <<>>} = utils:accum_n_bytes(3, <<"abc">>, <<>>),
    {<<"abc">>, <<"d">>} = utils:accum_n_bytes(3, <<"abc">>, <<"d">>),
    {<<"abc">>, <<"de">>} = utils:accum_n_bytes(3, <<"abcd">>, <<"e">>).

find_byte__not_found__test() ->
    0 = utils:find_byte(99, <<>>),
    1 = utils:find_byte(99, <<1>>),
    2 = utils:find_byte(99, <<1, 2>>).

find_byte__found__test() ->
    0 = utils:find_byte(99, <<99>>),
    0 = utils:find_byte(99, <<99, 1>>),
    1 = utils:find_byte(99, <<1, 99>>),
    2 = utils:find_byte(99, <<1, 2, 99, 3>>),
    ok.

extract_message__more__test() ->
    {more, <<>>} = utils:extract_message(<<>>),
    {more, <<1>>} = utils:extract_message(<<1>>),
    {more, <<0>>} = utils:extract_message(<<0>>),
    {more, <<0, 1>>} = utils:extract_message(<<0, 1>>),
    {more, <<0, "hello">>} = utils:extract_message(<<0, "hello">>),
    ok.

extract_message__ok__test() ->
    {ok, <<"hello">>, <<>>} = utils:extract_message(<<0, "hello", 255>>),
    {ok, <<"hello">>, <<"rest">>} = utils:extract_message(<<0, "hello", 255, "rest">>),
    ok.
