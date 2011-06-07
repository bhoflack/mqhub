-module(mqhub_message_fsm).
-behaviour(gen_fsm).
-include("mqhub.hrl").

%% API
-export([put/3,
         get/2]).

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
                key,
                value,
                preflist,
                num=0,
                replies=[]}).

%%% ==========================================================
%%% API
%%% ==========================================================
start_link(ReqId, From, Operation, Key) ->
    start_link(ReqId, From, Operation, Key, undefined).

start_link(ReqId, From, Operation, Key, Value) ->
    gen_fsm:start_link(?MODULE, [ReqId, From, Operation, Key, Value], []).

put(From, Key, Value) ->
    ReqId = mk_reqid(),
    ?PRINT(ReqId),
    start_link(ReqId, From, put, Key, Value).

get(From, Key) ->
    ReqId = mk_reqid(),
    ?PRINT(ReqId),
    start_link(ReqId, From, get, Key).

%%% ==========================================================
%%% States
%%% ==========================================================
init([ReqId, From, Operation, Key, Value]) ->
    State = #state{req_id=ReqId,
                   from=From,
                   op=Operation,
                   key=Key,
                   value=Value},
    ?PRINT(State),
    {ok, prepare, State, 0}.

prepare(timeout, State0=#state{key=Key}) ->
    DocIdx = riak_core_util:chash_key({<<"message">>,
                                       list_to_binary(Key)}),
    PrefList = riak_core_apl:get_apl(DocIdx, ?N, mqhub_message),
    State=State0#state{preflist=PrefList},
    ?PRINT(State),
    {next_state, execute, State, 0}.

execute(timeout, State0=#state{req_id=ReqId,
                               preflist=PrefList,
                               op=Op,
                               key=Key,
                               value=Value}) ->
    ?PRINT({execute, State0}),
    case Value of
        undefined ->
            mqhub_message_vnode:Op(PrefList, ReqId, Key);
        _ ->
            mqhub_message_vnode:Op(PrefList, ReqId, Key, Value)
    end,
    {next_state, waiting, State0}.

waiting({ok, ReqID}, State0=#state{from=From, num=Num0, key=Key}) ->
    Num = Num0 + 1,
    ?PRINT(Num),
    State = State0#state{num=Num},
    if
        Num =:= ?W ->
            From ! {ReqID, ok, Key},
            {stop, normal, State};
        true -> {next_state, waiting, State}
    end;
waiting({ok, ReqID, Value}, State0=#state{from=From, num=Num0, replies=Replies}) ->
    Num = Num0 + 1,
    State = State0#state{num=Num},
    if
        Num =:= ?R ->
            %% TODO improve handling multiple results ( check md5sum? )
            Reply =
                case lists:any(different(Value), Replies) of
                    true ->
                        Replies;
                    false ->
                        Value
                end,
            From ! {ReqID, ok, Reply},
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


%%%===================================================================
%%% Internal Functions
%%%===================================================================
different(A) -> fun(B) -> A =/= B end.

mk_reqid() -> erlang:phash2(erlang:now()).
