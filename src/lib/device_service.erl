-module(device_service).
-export([tokens/0, delete_device_with_gcm_token/1, persist_gcm_token/1, update_gcm_token/2, process_token_status_list/2]).

%%%============================================================================
%%% API
%%%============================================================================

% Returns all active tokens
tokens() ->
  Devices = boss_db:find(device, [{is_removed, false}]),
  Tokens = lists:map(fun(Device) -> Device:gcm_token() end, Devices),
  Tokens.

% Persists a new device with the received token. 
persist_gcm_token(Token) ->
  update_gcm_token(Token, Token).

% Persists or updates a device with the received token. 
% Returns ok | {error, ReasonString}
update_gcm_token(Token, NewToken) when is_list(Token), is_list(NewToken) ->
  Device = case boss_db:find(device, [{gcm_token, Token}]) of
    []                  -> boss_record:new(device, [{gcm_token, NewToken}]);
    [ExistingDevice]    -> 
      ExistingDevice:set([
          {previous_tokens, [Token | ExistingDevice:previous_tokens()]}, 
          {gcm_token, NewToken}
        ])
  end,
  save_device(Device);
update_gcm_token(_Token, _NewToken) ->
  {error, "Invalid token format"}.

% Deletes a token if it exists.
% Returns ok
delete_device_with_gcm_token(Token) ->
  case device_with_gcm_token(Token) of
    undefined -> ok;
    Device -> 
      UpdatedDevice = Device:set(is_removed, true),
      save_device(UpdatedDevice)
  end, 
  ok.


device_with_gcm_token(Token) ->
  boss_db:find_first(device, [{gcm_token, Token}]).


process_token_status_list(TokenStatusList, NotificationTemplate) ->
  io:format("Handle statuses from gcm: ~p", [TokenStatusList]),
  lists:foreach(fun(Entry) -> process_token(Entry, NotificationTemplate) end, TokenStatusList).

process_token({Token, {{ok, MessageId}, {new_token, NewToken}}}, NotificationTemplate) ->
  update_gcm_token(Token, NewToken),
  Device = device_with_gcm_token(NewToken),
  create_sent_notification(MessageId, Device, NotificationTemplate);
process_token({Token, {ok, MessageId}}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  create_sent_notification(MessageId, Device, NotificationTemplate);
process_token({Token, remove}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  create_sent_notification(remove, Device, NotificationTemplate),
  delete_device_with_gcm_token(Token);
process_token({Token, retry}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  create_sent_notification(retry, Device, NotificationTemplate);
process_token({Token, error}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  create_sent_notification(error, Device, NotificationTemplate).



%%%
%%% Internal
%%%


%TODO: Refactor into notification_service?
create_sent_notification(remove, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent - Status 'Invalid token'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(retry, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent -  Status 'Retry'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(error, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message not sent - Status 'Uknown Error'"}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save();
create_sent_notification(GcmMessageId, Device, NotificationTemplate) ->
  Notification = boss_record:new(notification, [{delivery_status, "Message sent"}, {gcm_message_id, GcmMessageId}, {device_id, Device:id()}, {notification_template_id, NotificationTemplate:id()}]),
  Notification:save().


% Returns ok | {error, ReasonString}
save_device(Device) ->
  case Device:save() of 
    {ok, _SavedDevice} -> ok;
    {error, [{_Key, Reason}|_T]} -> {error, Reason}
  end.
