-module(mqhub_queue_fsm).
-behaviour(gen_fsm).
-include("mqhub.hrl").

%% API
-export([start_link/4,
         create_queue/2,
         push/3,
         pull/2]).

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
                queue,
                arg,
                preflist,
                num=0,
                replies=[]}).

%%% ==========================================================
%%% API
%%% ==========================================================
start_link(ReqId, From, Queue, Operation) ->
    start_link(ReqId, From, Queue, Operation, undefined).

start_link(ReqId, From, Queue, Operation, Arg) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Operation, Queue, Arg], []).

create_queue(From, Name) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Name, create_queue).

push(From, Queue, Message) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Queue, push, Message).

pull(From, Queue) ->
    ReqId = mqhub_util:mk_reqid(),
    start_link(ReqId, From, Queue, pull).

%%% ==========================================================
%%% States
%%% ==========================================================
init([ReqId, From, Operation, Queue, Arg]) ->
    State = #state{req_id=ReqId,
                   from=From,
                   op=Operation,
                   queue=Queue,
                   arg=Arg},
    {ok, prepare, State, 0}.

prepare(timeout, State0=#state{queue=Queue}) ->
    DocIdx = riak_core_util:chash_key({<<"queue">>, Queue}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, mqhub_queue),
    State=State0#state{preflist=PrefList},
    {next_state, execute, State, 0}.

execute(timeout, State0=#state{req_id=ReqID,
                               preflist=PrefList,
                               op=Op,
                               queue=Queue,
                               arg=Arg}) ->
    case Arg of
        undefined ->
            mqhub_queue_vnode:Op(PrefList, ReqID, Queue);
        _ ->
            mqhub_queue_vnode:Op(PrefList, ReqID, Queue, Arg)
    end,
    {next_state, waiting, State0}.

waiting({ok, ReqID}, State0=#state{from=From, num=Num0}) ->
    Num = Num0 + 1,
    ?PRINT(Num),
    State = State0#state{num=Num},
    if
        Num =:= ?W ->
            From ! {ReqID, ok},
            {stop, normal, State};
        true -> {next_state, waiting, State}
    end;

waiting({Status, ReqID, Val}, State0=#state{op=pull, from=From, num=Num0, replies=Replies0}) ->
    Num = Num0 + 1,
    ?PRINT(Num),
    Replies = [Val|Replies0],
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
            From ! {ReqID, Status, Reply},
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
