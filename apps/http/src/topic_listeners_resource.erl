-module(topic_listeners_resource).
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
    {['PUT', 'GET', 'DELETE'], ReqData, Ctx}.

content_types_accepted(ReqData, State) ->
    {[{"application/json", from_json}], ReqData, State}.

content_types_provided(ReqData, State) ->
    {[{"application/json", to_json}], ReqData, State}.


from_json(ReqData, Ctx) ->
    Topic = topic(ReqData),
    Listener = wrq:req_body(ReqData),
    mqhub:subscribe(Topic, Listener),
    {ok, ReqData, Ctx}.

to_json(ReqData0, Ctx) ->
    Topic = topic(ReqData0),
    ReqData = wrq:set_resp_header("Pragma", "no-cache", ReqData0),
    {ok, Listeners} = mqhub:listeners(Topic),
    {mochijson2:encode(Listeners), ReqData, Ctx}.

topic(ReqData) ->
    Username = wrq:path_info(username, ReqData),
    TopicName = wrq:path_info(topic, ReqData),
    list_to_binary("/" ++ Username ++ "/topic/" ++ TopicName).

