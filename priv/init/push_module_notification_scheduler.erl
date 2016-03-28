-module(push_module_notification_scheduler).
-export([init/0, stop/1, check_expired_and_unsent_notifications/0]).
-define(INTERVAL, 20000).

init() ->
  register(gcm_message_sender, spawn(gcm_message_sender, init, [])),
  {ok, TimerId} = timer:apply_interval(?INTERVAL, ?MODULE, check_expired_and_unsent_notifications, []),
  {ok, [TimerId]}.

stop(TimerIds) ->
  gcm_message_sender ! stop,
  lists:foreach(fun(TimerId) -> timer:cancel(TimerId) end, TimerIds).


check_expired_and_unsent_notifications() ->
  ExpiredNotifications = notification_service:expired_notification_templates(),
  lists:foreach(fun send_scheduled_notification/1, ExpiredNotifications).

send_scheduled_notification(NotificationTemplate) ->
  UpdatedTemplate = notification_service:update_notification_template(NotificationTemplate, sent_at, calendar:local_time()),
  gcm_message_sender ! {send, UpdatedTemplate}.
