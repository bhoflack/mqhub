-module(mqhub_queue_vnode).
-behaviour(riak_core_vnode).
-include("mqhub.hrl").

-export([create_queue/3,
         push/4,
         pull/3]).

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

-record(state, {partition, queues, messages}).
-include_lib("riak_core/include/riak_core_vnode.hrl").

-define(MASTER, mqhub_queue_vnode_master).
-define(sync(PrefList, Command, Master),
        riak_core_vnode_master:sync_command(PrefList, Command, Master)).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, queues=dict:new()}}.

create_queue(Preflist, ReqID, Queue) ->
    riak_core_vnode_master:command(Preflist,
                                   {create_queue, ReqID, Queue},
                                   {fsm, undefined, self()},
                                   ?MASTER).

push(Preflist, ReqID, Queue, Message) ->
    riak_core_vnode_master:command(Preflist,
                                   {push, ReqID, Queue, Message},
                                   {fsm, undefined, self()},
                                   ?MASTER).

pull(Preflist, ReqID, Queue) ->
    riak_core_vnode_master:command(Preflist,
                                   {pull, ReqID, Queue},
                                   {fsm, undefined, self()},
                                   ?MASTER).

handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command({create_queue, ReqID, Queue}, _Sender, #state{queues=Queues}=State) ->
    {reply, {ok, ReqID}, State#state{queues=dict:store(Queue, [], Queues)}};
handle_command({push, ReqID, Queue, Message}, _Sender, #state{queues=Queues}=State) ->
    ?PRINT({pushing, Queue, Message}),
    QueuedMessages = case dict:find(Queue, Queues) of
                         error -> dict:store(Queue, [Message], Queues);
                         _ -> dict:append_list(Queue, [Message], Queues)
                     end,
    ?PRINT({queuedMessages, QueuedMessages}),
    {reply, {ok, ReqID}, State#state{queues=QueuedMessages}};
handle_command({pull, ReqID, Queue}, _Sender, #state{queues=Queues}=State) ->
    {ok, QueuedMessages} =
        case dict:find(Queue, Queues) of
            error -> not_found;
            Messages -> Messages
        end,
    {reply, {ok, ReqID, QueuedMessages}, State#state{queues=dict:store(Queue, [], Queues)}};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.queues),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{queues=Queues0}=State) ->
    {Queue, Messages} = binary_to_term(Data),
    Queues = dict:store(Queue, Messages, Queues0),
    {reply, ok, State#state{queues=Queues}}.

encode_handoff_item(Queue, Messages) ->
    term_to_binary({Queue, Messages}).

is_empty(State) ->
    case dict:size(State#state.queues) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State#state{queues=dict:new()}}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
