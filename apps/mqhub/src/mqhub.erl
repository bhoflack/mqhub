-module(mqhub).
-include("mqhub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-compile({no_auto_import,[get/1]}).

-export([
         ping/0,
         create_queue/1,
         push/2,
         pull/1,
         create_topic/1,
         subscribe/2,
         unsubscribe/2,
         publish/2,
         listeners/1
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
    push_key(Name, Key).

pull(Name) ->
    case mqhub_util:with_command(fun() -> mqhub_queue_fsm:pull(self(), Name) end) of
        {ok, Refs} -> {ok, lists:map(fun(Ref) -> {ok, Msg} = get(Ref), Msg end, Refs)};
        {error, Reason} -> {error, Reason}
    end.

create_topic(Topic) ->
    mqhub_util:with_command(fun() -> mqhub_topic_fsm:create_topic(self(), Topic) end).

subscribe(Topic, Listener) ->
    mqhub_util:with_command(fun() -> mqhub_topic_fsm:subscribe(self(), Topic, Listener) end).

unsubscribe(Topic, Listener) ->
    mqhub_util:with_command(fun() -> mqhub_topic_fsm:unsubscribe(self(), Topic, Listener) end).

publish(Topic, Message) ->
    case listeners(Topic) of
        {ok, Listeners} ->
            {ok, Key} = put(Message),
            lists:map(fun(Listener) -> push_key(Listener, Key) end, Listeners);
        {error, Reason} -> {error, Reason}
    end.

%% not exported
push_key(Name, Key) ->
    mqhub_util:with_command(fun() -> mqhub_queue_fsm:push(self(), Name, Key) end).

listeners(Topic) ->
    mqhub_util:with_command(fun() -> mqhub_topic_fsm:listeners(self(), Topic) end).

%% Private API
get(Key) ->
    {ok, Value} = mqhub_util:with_command(fun() -> mqhub_message_fsm:get(self(), Key) end),
    Value.

put(Value) ->
    Key = mqhub_util:md5(Value),
    mqhub_util:with_command(fun() -> mqhub_message_fsm:put(self(), Key, Value) end).
