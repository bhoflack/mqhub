-module(mqhub_util).
-export([md5/1,
         with_command/1,
         different/1,
         mk_reqid/0]).
-define(TIMEOUT, 5000).

md5(S) ->
 string:to_upper(
  lists:flatten([io_lib:format("~2.16.0b",[N]) || <<N>> <= erlang:md5(S)])).

with_command(C) ->
    C(),
    receive
        {_ReqID, Status} -> Status;
        {_ReqID, Status, Val} -> {Status, Val}
    after ?TIMEOUT ->
            {error, timeout}
    end.

different(A) -> fun(B) -> A =/= B end.

mk_reqid() -> erlang:phash2(erlang:now()).
