-module(topic_listener_resource).
-export([init/1,
         allowed_methods/2,
         delete_resource/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
    {ok, []}.

allowed_methods(ReqData, Ctx) ->
    {['DELETE'], ReqData, Ctx}.

delete_resource(ReqData, State) ->
    Topic = topic(ReqData),
    Listener = list_to_binary("/" ++ wrq:disp_path(ReqData)),
    mqhub:unsubscribe(Topic, Listener),
    {true, ReqData, State}.

topic(ReqData) ->
    Username = wrq:path_info(username, ReqData),
    TopicName = wrq:path_info(topic, ReqData),
    list_to_binary("/" ++ Username ++ "/topic/" ++ TopicName).

