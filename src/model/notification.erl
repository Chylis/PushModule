-module(notification, [
    Id, 
    DeliveryStatus,         % Status string, 'Sent' | 'Unknown Error' | 'Retry' | 'Invalid Token'
    GcmMessageId,           % Gcm message id of the notification
    DeviceId,               % Id of device/token that received notfication
    NotificationTemplateId  % Id of notification template containing the content
  ]).
-compile(export_all).
-belongs_to(notification_template).
-belongs_to(device).


%%%============================================================================
%%% API
%%%============================================================================

is_sent() ->
  DeliveryStatus == "Sent".
