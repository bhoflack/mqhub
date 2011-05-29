-module(mqhub_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    case mqhub_sup:start_link() of
        {ok, Pid} ->
            ok = riak_core:register_vnode_module(mqhub_queue_vnode),
            ok = riak_core_node_watcher:service_up(mqhub_queue, self()),

            ok = riak_core:register_vnode_module(mqhub_message_vnode),
            ok = riak_core_node_watcher:service_up(mqhub_message, self()),

            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

stop(_State) ->
    ok.
