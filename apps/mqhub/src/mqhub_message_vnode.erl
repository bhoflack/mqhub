-module(mqhub_message_vnode).
-behaviour(riak_core_vnode).
-include("mqhub.hrl").

-export([get/2,
         put/3]).

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

-include_lib("riak_core/include/riak_core_vnode.hrl").

-record(state, {partition, messages}).

-define(MASTER, rts_stat_vnode_master).
-define(sync(PrefList, Command, Master),
        riak_core_vnode_master:sync_command(PrefList, Command, Master)).

%% API
start_vnode(I) ->
    riak_core_vnode_master:get_vnode_pid(I, ?MODULE).

init([Partition]) ->
    {ok, #state{partition=Partition, messages=dict:new()}}.

put(IdxNode, Key, Message) ->
    ?sync(IdxNode, {put, Key, Message}, ?MASTER).

get(IdxNode, Key) ->
    ?sync(IdxNode, {get, Key}, ?MASTER).

handle_command(ping, _Sender, State) ->
    {reply, {pong, State#state.partition}, State};
handle_command({save, Key, Message}, _Sender, #state{messages=Messages}=State) ->
    {reply, ok, State#state{messages=dict:store(Key, Message, Messages)}};
handle_command({get, Key}, _Sender, #state{messages=Messages}=State) ->
    Reply =
        case dict:find(Key, Messages) of
            error ->
                not_found;
            Found ->
                Found
        end,
    {reply, Reply, State};
handle_command(Message, _Sender, State) ->
    ?PRINT({unhandled_command, Message}),
    {noreply, State}.

handle_handoff_command(?FOLD_REQ{foldfun=Fun, acc0=Acc0}, _Sender, State) ->
    Acc = dict:fold(Fun, Acc0, State#state.messages),
    {reply, Acc, State}.

handoff_starting(_TargetNode, State) ->
    {true, State}.

handoff_cancelled(State) ->
    {ok, State}.

handoff_finished(_TargetNode, State) ->
    {ok, State}.

handle_handoff_data(Data, #state{messages=Messages0}=State) ->
    {Key, Value} = binary_to_term(Data),
    Messages = dict:store(Key, Value, Messages0),
    {reply, ok, State#state{messages=Messages}}.

encode_handoff_item(Key, Value) ->
    term_to_binary({Key, Value}).

is_empty(State) ->
    case dict:size(State#state.messages) of
        0 -> {true, State};
        _ -> {false, State}
    end.

delete(State) ->
    {ok, State#state{messages=dict:new()}}.

handle_exit(_Pid, _Reason, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.
