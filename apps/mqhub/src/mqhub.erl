-module(mqhub).
-include("mqhub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-export([
         ping/0,
         create_queue/1
        ]).

%% Public API

% @doc Pings a random vnode to make sure communication is functional
ping() ->
    DocIdx = riak_core_util:chash_key({<<"ping">>, term_to_binary(now())}),
    PrefList = riak_core_apl:get_primary_apl(DocIdx, 1, mqhub),
    [{IndexNode, _Type}] = PrefList,
    riak_core_vnode_master:sync_spawn_command(IndexNode, ping, mqhub_vnode_master).

create_queue(Name) ->
    DocIdx = riak_core_util:chash_key({<<"queue">>, term_to_binary(Name)}),
    PrefList = riak_core_apl:get_apl(DocIdx, 1, mqhub_queue),
    [IdxNode] = PrefList,
    rts_entry_vnode:create_queue(IdxNode, Name).
