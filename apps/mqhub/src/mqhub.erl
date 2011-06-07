-module(mqhub).
-include("mqhub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-compile({no_auto_import,[get/1]}).

-export([
         ping/0,
         create_queue/1,
         push/2,
         pull/1
        ]).

%% Public API

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, mqhub),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, mqhub_vnode_master).

create_queue(Name) ->
    mqhub_util:with_command(fun() -> mqhub_queue_fsm:create_queue(self(), Name) end).

push(Name, Message) ->
    {ok, Key} = put(Message),
    mqhub_util:with_command(fun() -> mqhub_queue_fsm:push(self(), Name, Key) end).

pull(Name) ->
    {ok, Refs} = mqhub_util:with_command(fun() -> mqhub_queue_fsm:pull(self(), Name) end),
    lists:map(fun(Ref) -> get(Ref) end, Refs).

get(Key) ->
    {ok, Value} = mqhub_util:with_command(fun() -> mqhub_message_fsm:get(self(), Key) end),
    Value.

put(Value) ->
    Key = mqhub_util:md5(Value),
    mqhub_util:with_command(fun() -> mqhub_message_fsm:put(self(), Key, Value) end).
