
-module(utils).

-export([accum_n_bytes/3, find_byte/2, find_byte/3, extract_message/1]).

accum_n_bytes(N, State, Data) ->
    NewState = <<State/binary, Data/binary>>,
    case NewState of
        <<Result:N/binary, Rest/binary>> ->
            {Result, Rest};
        _ ->
            NewState
    end.

find_byte(B, <<B:8, _Rest/binary>>, Result) ->
    Result;
find_byte(_B, <<>>, Result) ->
    Result;
find_byte(B, <<_:8, Rest/binary>>, Result) ->
    find_byte(B, Rest, Result+1).
find_byte(B, Data) when is_binary(Data) ->
    find_byte(B, Data, 0).

extract_message(<<0:8, Rest/binary>>=Data) ->
    Size = size(Rest),
    case find_byte(255, Rest) of
        Size ->
            {more, Data};
        N ->
            <<Message:N/binary, 255, NewRest/binary>> = Rest,
            {ok, Message, NewRest}
    end;
extract_message(Data) ->
    {more, Data}.
    
