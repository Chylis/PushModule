-module(gcm_api).
-export([send_message/4]).

-ifdef(TEST).
-compile(export_all).
-endif.

%%%
%%% API
%%%

% Attemps to send a message to GCM.
% Returns a list of tuples, where each tuple is a token and its gcm status.
% - {ok, [token, status]}, e.g. [{"12", ok} {"34", remove}, {"56", retry}, {"78", {replace, "87"}] 
% - {error {retry, Duration}} 
% - {error, Reason}
send_message(ApiKey, ReceiverTokens, MsgTitle, MsgBody) ->
  Request = create_request(ApiKey, ReceiverTokens, MsgTitle, MsgBody),

  case http_utils:send_http_request(post, Request, [], []) of
    {ok, {200, Response}} -> 
      handle_response(Response, ReceiverTokens);
    {ok, {200, Headers, Response}} -> 
      handle_response(Response, ReceiverTokens);
    {ok, {500, Headers, _}} -> 
      {error, {retry, duration}}; % Retryn after 'retry-after' header (if present), and implement exponential-backoff in retry mechanism.
    Other -> 
      {error, Other}
  end.


%%%
%%% Internal
%%%

% Creates a GCM http request
create_request(ApiKey, ReceiverTokens, MsgTitle, MsgBody) ->
  ApiKeyEntry = string:concat("key=", ApiKey),
  Headers = [{"Authorization", ApiKeyEntry}],
  ContentType = "application/json",
  BaseUrl = "https://gcm-http.googleapis.com/gcm/send",
  Body = create_payload(ReceiverTokens, MsgTitle, MsgBody),
  {BaseUrl, Headers, ContentType, Body}.


create_payload(ReceiverTokens, MsgTitle, MsgBody) ->
  Receiver = create_payload_receiver(ReceiverTokens),
  lists:flatten(mochijson:encode({struct, [
                                           Receiver,
                                           {priority, high},
                                           {notification, {struct, [ 
                                                                    {sound, default},
                                                                    {title, MsgTitle},
                                                                    {body, MsgBody}
                                                                   ]}}
                                          ]})).

create_payload_receiver([ReceiverToken]) ->
  {to, ReceiverToken};
create_payload_receiver(ReceiverTokens)  ->
  {registration_ids, {array, ReceiverTokens}}.


% Handles the GCM response.
% Returns a list of tuples, where each tuple is a token and its gcm status.
% E.g. [{"12", ok} {"34", remove}, {"56", {retry, 60}}, {"78", {replace, "87"} ]

% Param Response = 
% {struct,[
%         {"multicast_id",8767287531181127482},
%         {"success",1},
%         {"failure",1},
%         {"canonical_ids",0},
%         {"results",
%          {array,[
%                  {struct,[{"error","InvalidRegistration"}]},
%                  {struct,[{"message_id", "0:1457282602303139%1391a0131391a013"}]}
%                 ]}}
%        ]}}
handle_response(RawResponse, OriginalTokens) ->
  {struct, Response} = mochijson:decode(RawResponse),
  Success = proplists:get_value("failure", Response) == 0 andalso proplists:get_value("canonical_ids", Response) == 0,
  if 
    Success == false ->
       {array, Results} = proplists:get_value("results", Response),
       HandledResults = handle_results(Results),
       TokenStatusList = lists:zip(OriginalTokens, HandledResults),
       {ok, TokenStatusList};
    true ->
      {ok, []}
  end.

% Goes through each result from the gcm response body and checks its status.
% Returns a list of statuses for each individual result. 
% Each status in the list can be mapped by index to the token that was originally used when sending the request.
handle_results(Results) ->
  handle_results(Results, []).

handle_results([], HandledResults) ->
  lists:reverse(HandledResults);
handle_results([ {struct, Result} | T ], HandledResults) ->
  HandledResult = handle_result(Result),
  handle_results(T, [HandledResult | HandledResults]);
handle_results([ {array, Result } | T ], HandledResults) ->
  HandledResult = handle_result(Result),
  handle_results(T, [HandledResult | HandledResults]).

% Handles a single result entry.
% Returns: ok | remove | {retry, durationSeconds} | {replace, <NewToken>}
handle_result([{struct, [{"message_id", _}]}, {struct, [{"registration_id", NewId}]}]) ->
  {replace, NewId};
handle_result([{"error", "Unavailable"}]) ->
  retry;
handle_result([{"error", "InternalServerError"}]) ->
  retry;
handle_result([{"error", "NotRegistered"}]) ->
  remove;
handle_result([{"error", "InvalidRegistration"}]) ->
  remove;
handle_result(Result) ->
  ok.

