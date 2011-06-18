-module(mqhub_topic_fsm).
-behaviour(gen_fsm).
-include("mqhub.hrl").

%% API
-export([create_topic/2,
         subscribe/3,
         unsubscribe/3,
         listeners/2]).

%% Callbacks
-export([init/1,
         code_change/4,
         handle_event/3,
         handle_info/3,
         handle_sync_event/4,
         terminate/3]).

%% States
-export([prepare/2,
         execute/2,
         waiting/2]).

-record(state, {req_id,
                from,
                op,
                topic,
                arg,
                preflist,
                num=0,
                replies=[]}).


%%% ==========================================================
%%% API
%%% ==========================================================
create_topic(From, Topic) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Topic, create_topic).

subscribe(From, Topic, Listener) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Topic, subscribe, Listener).

unsubscribe(From, Topic, Listener) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Topic, unsubscribe, Listener).

listeners(From, Topic) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Topic, listeners).

start_link(ReqId, From, Topic, Operation) ->
    start_link(ReqId, From, Topic, Operation, undefined).

start_link(ReqId, From, Topic, Operation, Arg) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Topic, Operation, Arg], []).

%%% ==========================================================
%%% States
%%% ==========================================================
init([ReqId, From, Topic, Operation, Arg]) ->
    State = #state{req_id=ReqId,
                   from=From,
                   op=Operation,
                   topic=Topic,
                   arg=Arg},
    {ok, prepare, State, 0}.

prepare(timeout, State0=#state{topic=Topic}) ->
    DocIdx = riak_core_util:chash_key({<<"topic">>, Topic}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, mqhub_topic),
    State = State0#state{preflist=PrefList},
    {next_state, execute, State, 0}.

execute(timeout, State0=#state{req_id=ReqId,
                               preflist=PrefList,
                               op=Op,
                               topic=Topic,
                               arg=Arg}) ->
    case Arg of
        undefined ->
            mqhub_topic_vnode:Op(PrefList, ReqId, Topic);
        _ ->
            mqhub_topic_vnode:Op(PrefList, ReqId, Topic, Arg)
    end,
    {next_state, waiting, State0}.

waiting({ok, ReqID}, State0=#state{from=From, num=Num0}) ->
    Num = Num0 + 1,
    ?PRINT({waiting, Num}),
    State = State0#state{num=Num},
    if
        Num =:= ?W ->
            From ! {ReqID, ok},
            {stop, normal, State};
        true -> {next_state, waiting, State}
    end;
waiting({Status, ReqId, Val}, State0=#state{from=From, num=Num0, replies=Replies0}) ->
    Num = Num0 + 1,
    ?PRINT({waiting, Num}),
    Replies = [Val | Replies0],
    State = State0#state{num=Num, replies=Replies},
    if
        Num =:= ?R ->
            Reply =
                case lists:any(mqhub_util:different(Val), Replies) of
                    true ->
                        Replies;
                    false ->
                        Val
                end,
            ?PRINT({reply, Reply, to, From}),
            From ! {ReqId, Status, Reply},
            {stop, normal, State};
        true -> {next_state, waiting, State}
    end.

handle_info(_Info, _StateName, StateData) ->
    {stop, badmsg, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

terminate(_Reason, _SN, _SD) ->
    ok.
