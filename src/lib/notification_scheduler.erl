-module(notification_scheduler).
-export([check_expired_and_unsent_notifications/0]).
-include("include/defines.hrl").

% Check for Pending notification every X ms
-define(INTERVAL, 10000).

% Max number of retries to send a notification
-define(MAX_RETRIES, 5).

% Max number of milliseconds to wait before next retry (8 Hours)
-define(CAP_RETRY_AFTER_MILLISECONDS, 28800000).

check_expired_and_unsent_notifications() ->
  timer:sleep(?INTERVAL),
  ExpiredNotifications = notification_service:expired_notification_templates(),
  lists:foreach(fun send_scheduled_notification/1, ExpiredNotifications),
  check_expired_and_unsent_notifications().

send_scheduled_notification(NotificationTemplate) ->
  UpdatedTemplate = notification_service:update_notification_template(NotificationTemplate, status, "Sending"),

  AllReceivers = device_service:tokens(),
  SplitReceiverLists = list_utils:n_length_chunks(AllReceivers, 1000),

  ResultList = lists:map(fun(Receivers) -> 
        FilteredReceivers = lists:filter(fun(Token) -> Token =/= undefined end, Receivers),
        send_message(FilteredReceivers, UpdatedTemplate)
    end, 
    SplitReceiverLists),

  case lists:keyfind(error, 1, ResultList) of
    false ->
      notification_service:update_notification_template(UpdatedTemplate, [{status, "Sent"}, {sent_at, date_utils:local_datetime()}]);
    {error, StatusString} ->
      notification_service:update_notification_template(UpdatedTemplate, status, StatusString)
  end.

send_message(ReceiverTokens, NotificationTemplate) ->
  send_message(ReceiverTokens, NotificationTemplate, 0, 0).

send_message(_ReceiverTokens, NotificationTemplate, Attempt, _RetryAfterMilliSeconds) when Attempt >= ?MAX_RETRIES ->
  io:format("~n~s: Reached max number of retries (~p) trying to send '~p'~n", [date_utils:format_utc_timestamp(), ?MAX_RETRIES, NotificationTemplate]),
  {error, "Max number of retries"};

send_message(ReceiverTokens, NotificationTemplate, Attempt, RetryAfterMilliSeconds) ->
  apply_exponential_backoff(RetryAfterMilliSeconds, Attempt),

  io:format("~n~s: Attempt '~p' for '~p'.~n", [date_utils:format_utc_timestamp(), Attempt, NotificationTemplate]),
  case gcm_api:send_message(?GCM_API_KEY, ReceiverTokens, NotificationTemplate:title(), NotificationTemplate:body()) of
    
    {ok, {token_statuses, TokenStatusList}, {retry_tokens, []}, {retry_after, _RetryAfter}} ->
      device_service:process_token_status_list(TokenStatusList, NotificationTemplate),
      ok;

    {ok, {token_statuses, TokenStatusList}, {retry_tokens, TokensToRetry}, {retry_after, RetryAfter}} ->
      device_service:process_token_status_list(TokenStatusList, NotificationTemplate),
      send_message(TokensToRetry, NotificationTemplate, Attempt+1, RetryAfter*1000);

    {error, {retry_after, RetryAfter}} ->
      send_message(ReceiverTokens, NotificationTemplate, Attempt+1, RetryAfter*1000);

    {error, Reason} ->
      io:format("~nFailed sending notification: Received error: '~p'~n", [Reason]),
      StatusString = io_lib:format("Error: ~p", [Reason]),
      {error, StatusString}
  end.

apply_exponential_backoff(RetryAfterMilliSeconds, Attempt) ->
  NextSleepDuration = erlang:round(RetryAfterMilliSeconds * math:pow(2, Attempt) / 2),
  TimeToSleep = erlang:min(?CAP_RETRY_AFTER_MILLISECONDS, NextSleepDuration),
  io:format("~n~s: GCM Message queue going to sleep for '~p' ms~n", [date_utils:format_utc_timestamp(), TimeToSleep]),
  timer:sleep(TimeToSleep).
