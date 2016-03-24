-module(push_module_notification_scheduler).
-export([init/0, stop/1, check_expired_and_unsent_notifications/0]).
-include("include/defines.hrl").
-define(INTERVAL, 20000).

init() ->
  {ok, TimerId} = timer:apply_interval(?INTERVAL, ?MODULE, check_expired_and_unsent_notifications, []),
  {ok, [TimerId]}.

stop(TimerIds) ->
  lists:foreach(fun(TimerId) -> timer:cancel(TimerId) end, TimerIds).


check_expired_and_unsent_notifications() ->
  ExpiredNotifications = notification_service:expired_notification_templates(),
  lists:foreach(fun(NotificationTemplate) -> send_scheduled_notification(NotificationTemplate) end, ExpiredNotifications).

send_scheduled_notification(NotificationTemplate) ->
  Updated1 = notification_service:update_notification_template(NotificationTemplate, sent_at, calendar:local_time()),

  case gcm_api:send_message(?GCM_API_KEY, device_service:tokens(), NotificationTemplate:title(), NotificationTemplate:body()) of
    {ok, {token_statuses, TokenStatusList}, {retry_after, _RetryAfter}} ->
      device_service:process_token_status_list(TokenStatusList, NotificationTemplate),
      notification_service:update_notification_template(Updated1, status, "Sent");

    {error, {retry_after, _RetryAfter}} ->
      notification_service:update_notification_template(Updated1, status, "Failed: Retry");

    {error, Reason} ->
      StatusString = io_lib:format("Error: ~p", [Reason]),
      notification_service:update_notification_template(Updated1, status, StatusString)
  end.
