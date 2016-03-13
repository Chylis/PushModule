-module(notification, [
    Id, 
    DeliveryStatus,         % Status string
    GcmMessageId,           % Gcm message id of the notification
    DeviceId,               % Id of device/token that received notfication
    NotificationTemplateId  % Id of notification template containing the content
  ]).
-compile(export_all).
-belongs_to(notification_template).
-belongs_to(device).
