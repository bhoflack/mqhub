-module(mqhub).
-include("mqhub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         create_queue/1,
         push/2,
         pull/1,
         get/1,
         put/1
        ]).

-define(TIMEOUT, 5000).


%% Public API

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, mqhub),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, mqhub_vnode_master).

create_queue(Name) ->
    with_command(fun() -> mqhub_queue_fsm:create_queue(self(), Name) end).

push(Name, Message) ->
    {ok, Key} = put(Message),
    with_command(fun() -> mqhub_queue_fsm:push(self(), Name, Key) end).

pull(Name) ->
    {ok, Refs} = with_command(fun() -> mqhub_queue_fsm:pull(self(), Name) end),
    lists:map(fun(Ref) -> mqhub:get(Ref) end, Refs).

get(Key) ->
    {ok, Value} = with_command(fun() -> mqhub_message_fsm:get(self(), Key) end),
    Value.

put(Value) ->
    Key = md5(Value),
    ?PRINT(Key),
    with_command(fun() -> mqhub_message_fsm:put(self(), Key, Value) end).

%% private functions
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
