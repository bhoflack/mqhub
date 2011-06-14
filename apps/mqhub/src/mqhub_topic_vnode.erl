-module(mqhub_topic_vnode).
-behaviour(riak_core_vnode).

-export([create_topic/3,
         subscribe/4,
         unsubscribe/4,
         listeners/3]).

-export([start_vnode/1,
         init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2,
         handle_exit/3]).

-record(state, {partition, topics}).
-include("mqhub.hrl").
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(MASTER, mqhub_topic_vnode_master).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, topics=dict:new()}}.

create_topic(Preflist, ReqID, Topic) ->
    ?PRINT({create_topic, Topic}),
    riak_core_vnode_master:command(Preflist,
                                   {create_topic, ReqID, Topic},
                                   {fsm, undefined, self()},
                                   ?MASTER).

subscribe(Preflist, ReqID, Topic, Listener) ->
    riak_core_vnode_master:command(Preflist,
                                   {subscribe, ReqID, Topic, Listener},
                                   {fsm, undefined, self()},
                                   ?MASTER).

unsubscribe(Preflist, ReqID, Topic, Listener) ->
    riak_core_vnode_master:command(Preflist,
                                   {unsubscribe, ReqID, Topic, Listener},
                                   {fsm, undefined, self()},
                                   ?MASTER).

listeners(Preflist, ReqID, Topic) ->
    riak_core_vnode_master:command(Preflist,
                                   {listeners, ReqID, Topic},
                                   {fsm, undefined, self()},
                                   ?MASTER).

%% Private
handle_command({create_topic, ReqID, Topic}, _Sender, #state{topics=Topics}=State) ->
    ?PRINT({create_topic, ReqID, Topic}),
    {reply, {ok, ReqID}, State#state{topics=dict:store(Topic, [], Topics)}};
handle_command({subscribe, ReqID, Topic, Listener}, _Sender, #state{topics=Topics}=State) ->
    case dict:find(Topic, Topics) of
        error -> {reply, {error, ReqID, topic_not_found}, State};
        _ -> {reply,
              {ok, ReqID},
              State#state{topics=dict:append_list(Topic, [Listener], Topics)}}
    end;
handle_command({unsubscribe, ReqID, Topic, Listener}, _Sender, #state{topics=Topics}=State) ->
    case dict:find(Topic, Topics) of
        error -> {reply, {error, ReqID, topic_not_found}, State};
        Listeners -> {reply,
                      {ok, ReqID},
                      State#state{topics=dict:append(Topic, lists:delete(Listener, Listeners), Topics)}}
    end;
handle_command({listeners, ReqID, Topic}, _Sender, #state{topics=Topics}=State) ->
    case dict:find(Topic, Topics) of
        error -> {reply, {error, ReqID, topic_not_found}, State};
        {ok, Listeners} -> {reply,
                      {ok, ReqID, Listeners},
                      State}
    end;

handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.topics),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{topics=Topics0}=State) ->
    {Topic, Messages} = binary_to_term(Data),
    Topics = dict:store(Topic, Messages, Topics0),
    {reply, ok, State#state{topics=Topics}}.

encode_handoff_item(Topic, Messages) ->
    term_to_binary({Topic, Messages}).

is_empty(State) ->
    case dict:size(State#state.topics) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State#state{topics=dict:new()}}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.







