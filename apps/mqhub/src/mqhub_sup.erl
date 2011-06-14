-module(mqhub_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(_Args) ->
    QueueMaster = { mqhub_vnode_queue_master,
                    {riak_core_vnode_master, start_link, [mqhub_queue_vnode]},
                    permanent, 5000, worker, [riak_core_vnode_master]},
    MessageMaster = { mqhub_vnode_message_master,
                      {riak_core_vnode_master, start_link, [mqhub_message_vnode]},
                      permanent, 5000, worker, [riak_core_vnode_master]},
    TopicMaster = { mqhub_vnode_topic_master,
                    {riak_core_vnode_master, start_link, [mqhub_topic_vnode]},
                    permanent, 5000, worker, [riak_core_vnode_master]},

    { ok,
        { {one_for_one, 5, 10},
          [QueueMaster, MessageMaster, TopicMaster]}}.
