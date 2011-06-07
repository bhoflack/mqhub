-module(mqhub_util).
-export([md5/1,
         with_command/1]).

-define(TIMEOUT, 5000).

md5(S) ->
 string:to_upper(
  lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])).

with_command(C) ->
    C(),
    receive
        {_ReqID, ok} -> ok;
        {_ReqID, ok, Val} -> {ok, Val}
    after ?TIMEOUT ->
            {error, timeout}
    end.
