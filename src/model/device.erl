-module(device, [
    Id,             % Database id
    DeviceId,       % Device id, UUID
    UsrId,          % Id of owner
    AdditionalInfo, % Optional json object received from the client app containing dynamic data about the device, e.g. "os", "osVersion", etc
    GcmToken,       % Current token, string
    PreviousTokens, % List of previous gcm tokens,
    CreatedAt,      % Date 
    UpdatedAt       % Date
  ]).

-compile(export_all).
-belongs_to(usr).
-has({notification, many}).

-define(DEVICE_TYPE, "deviceType").
-define(OS, "os").
-define(OS_VERSION, "osVersion").
-define(FRAMEWORK_VERSION, "frameworkVersion").

%%%============================================================================
%%% API
%%%============================================================================

device_type() ->
  value_for_property(?DEVICE_TYPE).
os() ->
  value_for_property(?OS).
os_version() ->
  value_for_property(?OS_VERSION).
framework_version() ->
  value_for_property(?FRAMEWORK_VERSION).

% Returns a list of all keys in the AdditionalInfo json object
property_keys() ->
  case request_utils:property_list_from_json(AdditionalInfo) of 
    undefined -> [];
    Plist -> proplists:get_keys(Plist)
  end.

value_for_property(Property) ->
  case request_utils:property_list_from_json(AdditionalInfo) of 
    undefined -> [];
    Plist -> proplists:get_value(Property, Plist)
  end.

formatted_created_at() ->
  date_utils:format_datetime(CreatedAt).

formatted_updated_at() -> 
  date_utils:format_datetime(UpdatedAt).

%%%============================================================================
%%% Validation 
%%%============================================================================

validation_tests() ->
  [
   { fun() -> is_list(DeviceId) andalso length(DeviceId) > 0 end,  {device_id, "DeviceId Required"} },
   { fun() -> is_list(UsrId) andalso length(UsrId) > 0 end,  {usr_id, "UserId Required"} }
  ].

%%%============================================================================
%%% Hooks 
%%%============================================================================

before_create() ->
  Now = date_utils:local_datetime(),
  Modified = set([{created_at, Now}, {updated_at, Now}, {previous_tokens, []}]),
  {ok, Modified}.

before_update() ->
  Now = date_utils:local_datetime(),
  Modified = set([{updated_at, Now}]),
  {ok, Modified}.
