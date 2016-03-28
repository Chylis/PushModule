-module(device_service).
-export([create_device/3, device_properties/1, tokens/0, delete_gcm_token/2, persist_gcm_token/2, process_token_status_list/2]).

%%%============================================================================
%%% Device API
%%%============================================================================


% Returns ok | {error, ReasonString}
create_device(ExternalUserId, DeviceId, AdditionalInfoJson) ->
  case user_service:user_with_external_id(ExternalUserId) of
    [] -> {error, "No user with received userId"};
    undefined -> {error, "No user with received userId"};
    User ->
      case boss_db:find(device, [{device_id, DeviceId}]) of
        [] -> 
          NewDevice = boss_record:new(device, [{device_id, DeviceId}, {usr_id, User:id()}, {additional_info, AdditionalInfoJson}]),
          save_device(NewDevice);

        [ExistingDevice] -> 
          case ExistingDevice:usr_id() == User:id() of
            true ->
              UpdatedDevice = ExistingDevice:set([{additional_info, AdditionalInfoJson}]),
              save_device(UpdatedDevice);
            false ->
              {error, "Device doesn't belong to received user"}
          end
      end
  end.


% Returns a list containing the set of all device property keys
device_properties(Devices) ->
  device_properties(Devices, sets:new()).

device_properties([], PropertySet) ->
  lists:sort(sets:to_list(PropertySet));
device_properties([Device|Devices], PropertySet) ->
  Properties = sets:from_list(Device:property_keys()),
  device_properties(Devices, sets:union([PropertySet, Properties])).


%%%============================================================================
%%% Token API
%%%============================================================================


% Returns all active tokens
tokens() ->
  Devices = boss_db:find(device, [{gcm_token, 'not_equals', undefined}]),
  Tokens = lists:map(fun(Device) -> Device:gcm_token() end, Devices),
  Tokens.

persist_gcm_token([], _DeviceId) ->
  {error, "Missing token"};
persist_gcm_token(Token, DeviceId) ->
  update_gcm_token(Token, Token, DeviceId).

% Persists or updates a device with the received token. 
% Returns ok | {error, ReasonString}
update_gcm_token(Token, NewToken, DeviceId) when is_list(Token), is_list(NewToken) ->
  case boss_db:find(device, [{device_id, DeviceId}]) of
    []                  -> {error, "No device with the received deviceId is registered"};
    [ExistingDevice]    -> 
      UpdatedDevice = ExistingDevice:set([{gcm_token, NewToken}]),
      save_device(UpdatedDevice)
  end;
update_gcm_token(_Token, _NewToken, _DeviceId) ->
  {error, "Invalid token format"}.

% Deletes a token if it exists.
% Returns ok
delete_gcm_token([], _DeviceId) ->
  ok;
delete_gcm_token(Token, DeviceId) ->
  case device_with_gcm_token(Token) of
    undefined -> ok;
    ExistingDevice    -> 
      case ExistingDevice:device_id() == DeviceId of
        false -> ok;
        true ->
          UpdatedDevice = ExistingDevice:set([{gcm_token, undefined}]),
          save_device(UpdatedDevice)
      end
  end, 
  ok.

device_with_gcm_token(Token) ->
  boss_db:find_first(device, [{gcm_token, Token}]).


process_token_status_list(TokenStatusList, NotificationTemplate) ->
  lists:foreach(fun(Entry) -> process_token(Entry, NotificationTemplate) end, TokenStatusList).

process_token({Token, {{ok, MessageId}, {new_token, NewToken}}}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  update_gcm_token(Token, NewToken, Device:device_id()),
  notification_service:create_sent_notification(MessageId, Device, NotificationTemplate);
process_token({Token, {ok, MessageId}}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  notification_service:create_sent_notification(MessageId, Device, NotificationTemplate);
process_token({Token, remove}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  notification_service:create_sent_notification(remove, Device, NotificationTemplate),
  delete_gcm_token(Token, Device:device_id());
process_token({Token, retry}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  notification_service:create_sent_notification(retry, Device, NotificationTemplate);
process_token({Token, error}, NotificationTemplate) ->
  Device = device_with_gcm_token(Token),
  notification_service:create_sent_notification(error, Device, NotificationTemplate).


%%%
%%% Internal
%%%

% Returns ok | {error, ReasonString}
save_device(Device) ->
  case Device:save() of 
    {ok, _SavedDevice} -> ok;
    {error, [{_Key, Reason}|_T]} -> {error, Reason}
  end.
