-module(queue_resource).
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         content_types_provided/2,
         from_json/2,
         to_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Ctx) ->
    {['PUT', 'GET'], ReqData, Ctx}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.

from_json(ReqData, Ctx) ->
    Queue = wrq:path(ReqData),
    Body = wrq:req_body(ReqData),
    mqhub:push(Queue, Body),
    {ok, ReqData, Ctx}.

to_json(ReqData, Ctx) ->
    Queue = wrq:path(ReqData),
    {ok, Messages} = mqhub:pull(Queue),
    mochijson2:encode(Messages),
    {ok, ReqData, Ctx}.
