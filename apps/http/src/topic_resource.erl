-module(topic_resource).
-export([init/1,
         allowed_methods/2,
         content_types_accepted/2,
         from_json/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Ctx) ->
    {['PUT'], ReqData, Ctx}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

from_json(ReqData, Ctx) ->
    Topic = list_to_binary(wrq:path(ReqData)),
    Body = wrq:req_body(ReqData),
    mqhub:publish(Topic, Body),
    {ok, ReqData, Ctx}.
