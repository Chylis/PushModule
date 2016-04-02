-module(notification_service).
-export([
    notification_template_with_id/1,
    create_notification_template/4, 
    update_notification_template/2,
    update_notification_template/3,
    delete_notification_template_with_id/1,
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


update_notification_template(Notification, PropertyList) ->
  UpdatedNotification = Notification:set(PropertyList),
  {ok, SavedNotification} = UpdatedNotification:save(),
  SavedNotification.

update_notification_template(NotificationTemplate, Key, Value) ->
  UpdatedNotification = NotificationTemplate:set(Key, Value),
  {ok, SavedNotification} = UpdatedNotification:save(),
  SavedNotification.


% Remove notification template if it is not already sent.
% Return ok | {error, Reason}
delete_notification_template_with_id(Id) ->
  Notification = notification_template_with_id(Id),
  case Notification:is_pending() of
    false -> {error, "Notification has already been sent"};
    true -> boss_db:delete(Id)
  end.


% Returns all notification templates that have expired with status "Pending"
expired_notification_templates() ->
  ExpiredNotifications = boss_db:find(
    notification_template, 
    [{scheduled_for, 'lt', date_utils:local_datetime()}, {status, 'equals', "Pending"}], 
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
  Notification = boss_record:new(notification, [{delivery_status, "Invalid token"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(error, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Unknown Error"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(GcmMessageId, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Sent"}, {gcm_message_id, GcmMessageId}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save().
