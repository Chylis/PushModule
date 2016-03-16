-module(user_service).
-export([all_users/0, create_user/2, update_user_with_external_id/2, user_with_id/1, user_with_external_id/1, user_properties/1]).

%%%============================================================================
%%% API
%%%============================================================================

% Returns all registered users
all_users() ->
  Users = boss_db:find(usr, []),
  Users.

user_with_id(Id) ->
  boss_db:find_first(usr, [{id, Id}]).

user_with_external_id(ExternalId) ->
  boss_db:find_first(usr, [{external_id, ExternalId}]).

% Persists a new user with the external user id. 
create_user(ExternalId, AdditionalInfoJson) ->
  update_user_with_external_id(ExternalId, AdditionalInfoJson).

% Persists or updates a user with the received external id. 
% Returns ok | {error, ReasonString}
update_user_with_external_id(ExternalId, AdditionalInfoJson) when is_list(ExternalId), is_list(AdditionalInfoJson) ->
  User = case boss_db:find(usr, [{external_id, ExternalId}]) of
    []                  -> boss_record:new(usr, [{external_id, ExternalId}, {additional_info, AdditionalInfoJson}]);
    [Existing]          -> Existing:set([{additional_info, AdditionalInfoJson}])
  end,
  save_user(User);
update_user_with_external_id(_,_) ->
  {error, "Invalid id/additionalInfo format"}.

% Returns a list containing the set of all user property keys
user_properties(Users) ->
  user_properties(Users, sets:new()).

%%%
%%% Internal
%%%

user_properties([], PropertySet) ->
  lists:sort(sets:to_list(PropertySet));
user_properties([User|Users], PropertySet) ->
  Properties = sets:from_list(User:property_keys()),
  user_properties(Users, sets:union([PropertySet, Properties])).

% Returns ok | {error, ReasonString}
save_user(User) ->
  case User:save() of 
    {ok, _SavedUser} -> ok;
    {error, [{_Key, Reason}|_T]} -> {error, Reason}
  end.
