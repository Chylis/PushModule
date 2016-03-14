-module(notification_service).
-export([
    notification_template_with_id/1,
    create_notification_template/4, 
    update_notification_template/3,
    create_sent_notification/3,
    expired_notification_templates/0, 
    all_notification_templates/0
  ]).

%%%
%%% API
%%%

notification_template_with_id(Id) ->
  boss_db:find_first(notification_template, [{id, Id}]).

% Persists a 'notification template', representing a notification payload
create_notification_template(Title, Body, ScheduledFor, CreatorId) ->
  Notification = boss_record:new(notification_template, [{title, Title}, {body, Body}, {scheduled_for, ScheduledFor}, {admin_id, CreatorId}]),
  Notification:save().

update_notification_template(NotificationTemplate, Key, Value) ->
  UpdatedNotification = NotificationTemplate:set(Key, Value),
  {ok, SavedNotification} = UpdatedNotification:save(),
  SavedNotification.




% Returns all notification templates that have expired but not yet been sent
expired_notification_templates() ->
  ExpiredNotifications = boss_db:find(
    notification_template, 
    [{scheduled_for, 'lt', calendar:local_time()}, {sent_at, 'equals', undefined}], 
    [{order_by, scheduled_for}, {descending, false}]),
  ExpiredNotifications.


% Returns all notification templates 
all_notification_templates() ->
  Notifications = boss_db:find(
    notification_template, 
    [],
    [{order_by, scheduled_for}, {descending, false}]),
  Notifications.


% Persists a 'notification' model representing a sent notification.
create_sent_notification(remove, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent - 'Invalid token'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(retry, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent - 'Retry'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(error, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent - 'Uknown Error'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(GcmMessageId, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message sent"}, {gcm_message_id, GcmMessageId}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save().
