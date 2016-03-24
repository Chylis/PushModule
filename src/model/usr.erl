-module(usr, [
    Id, 
    ExternalId::string(),     % External user id, received from the client application
    AdditionalInfo::string(), % Optional json object received from the client app containing dynamic data about the user, e.g. "firstName", "gender", etc
    CreatedAt::datetime(),      % Date 
    UpdatedAt::datetime()       % Date
  ]).
-compile(export_all).
-has({device, many}).

%%%============================================================================
%%% API
%%%============================================================================

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
   { fun() -> is_list(ExternalId) andalso length(ExternalId) > 0 end,  {external_id, "A user must have an external user id"} }
  ].

%%%============================================================================
%%% Hooks 
%%%============================================================================

before_create() ->
  Now = date_utils:local_datetime(),
  Modified = set([{created_at, Now}, {updated_at, Now}]),
  {ok, Modified}.

before_update() ->
  Now = date_utils:local_datetime(),
  Modified = set([{updated_at, Now}]),
  {ok, Modified}.
