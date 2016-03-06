-module(gcm_api).
-export([send_message/4]).

send_message(ApiKey, ReceiverTokens, MsgTitle, MsgBody) ->
  Request = create_gcm_request_payload(ApiKey, ReceiverTokens, MsgTitle, MsgBody),
  case http_utils:send_http_request(post, Request, [], []) of
    {ok, {200, ResponseBody}} -> {ok, mochijson:decode(ResponseBody)};
    Other                     -> {error, Other}
  end.

% Creates a GCM request payload
% TODO: pattern matchto see if token is list of several tokens or only one, and adjust request body accordingly
create_gcm_request_payload(ApiKey, ReceiverTokens, MsgTitle, MsgBody) ->
  ApiKeyEntry = string:concat("key=", ApiKey),
  Headers = [{"Authorization", ApiKeyEntry}],
  ContentType = "application/json",
  BaseUrl = "https://gcm-http.googleapis.com/gcm/send",
  Body = lists:flatten(mochijson:encode({struct, [
                                                  {registration_ids, {array, ReceiverTokens}},
                                                  {priority, high},
                                                  {notification, {struct, [ 
                                                                           {sound, default},
                                                                           {title, MsgTitle},
                                                                           {body, MsgBody}
                                                                          ]}}
                                                 ]})),
  {BaseUrl, Headers, ContentType, Body}.
