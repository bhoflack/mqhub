-module(queue_resource).
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         process_post/2,
         from_json/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Ctx) ->
    {['PUT', 'POST', 'GET'], ReqData, Ctx}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

from_json(ReqData, Ctx) ->
    push(ReqData, Ctx, ok).

process_post(ReqData, Ctx) ->
    push(ReqData, Ctx, true).

push(ReqData, Ctx, Response) ->
    Queue = list_to_binary(wrq:path(ReqData)),
    Body = wrq:req_body(ReqData),
    mqhub:push(Queue, Body),
    {Response, ReqData, Ctx}.

to_json(ReqData0, Ctx) ->
    Queue = list_to_binary(wrq:path(ReqData0)),
    ReqData = wrq:set_resp_header("Pragma", "no-cache", ReqData0),
    case mqhub:pull(Queue) of
        {ok, Messages} -> {mochijson2:encode(Messages), ReqData, Ctx};
        {error, Reason} -> {atom_to_list(Reason), ReqData, Ctx}
    end.
