-module(topic_resource).
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         process_post/2,
         from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Ctx) ->
    {['PUT', 'POST'], ReqData, Ctx}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

process_post(ReqData, Ctx) ->
    push(ReqData, Ctx, true).

from_json(ReqData, Ctx) ->
    push(ReqData, Ctx, ok).

push(ReqData, Ctx, Response) ->
    Topic = list_to_binary(wrq:path(ReqData)),
    Body = wrq:req_body(ReqData),
    mqhub:publish(Topic, Body),
    {Response, ReqData, Ctx}.
