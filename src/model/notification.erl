-module(notification, [
    Id, 
    DeliveryStatus::string(),         % Status string, 'Sent' | 'Unknown Error' | 'Invalid Token'
    GcmMessageId::string(),           % Gcm message id of the notification
    DeviceId::string(),               % Id of device/token that received notfication
    NotificationTemplateId::string()  % Id of notification template containing the content
  ]).
-compile(export_all).
-belongs_to(notification_template).
-belongs_to(device).


%%%============================================================================
%%% API
%%%============================================================================

is_sent() ->
  DeliveryStatus == "Sent".
