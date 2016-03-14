-module(notification_service).
-export([create_notification_template/3, expired_notification_templates/0]).

%%%
%%% API
%%%

create_notification_template(Title, Body, ScheduledFor) ->
  Notification = boss_record:new(notification_template, [{title, Title}, {body, Body}, {scheduled_for, ScheduledFor}]),
  Notification:save().

expired_notification_templates() ->
  ExpiredNotifications = boss_db:find(
    notification_template, 
    [{scheduled_for, 'lt', calendar:local_time()}, {sent_at, 'equals', undefined}], 
    [{order_by, scheduled_for}, {descending, false}]),
  ExpiredNotifications.
