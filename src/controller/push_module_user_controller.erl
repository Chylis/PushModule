-module(push_module_user_controller, [Req, _SessionId]).
-export([before_/3, register_user/3, view_users/3, view_user/3]).

before_("register_user", _Request, _PathParams) ->
  {ok, []};
before_(_,_,_) ->
  auth_lib:require_login(Req).

register_user('POST', [], _Admin) ->
  {ExternalUserId, AdditionalInfoJson}  = request_utils:params(["userId", "additionalInfo"], Req),
  case user_service:create_user(ExternalUserId, AdditionalInfoJson) of
    ok                      -> {200, [], []};
    {error, Reason}         -> {500, Reason, []}
  end.

view_users('GET', [], _Admin) ->
  Users = user_service:all_users(),
  {ok, [{users, Users}, {properties, user_service:user_properties(Users)}]}.

view_user('GET', [UserId], _Admin) ->
  User = user_service:user_with_id(UserId),
  Devices = User:device(),
  {ok, [
      {user, User}, 
      {user_properties, user_service:user_properties([User])}, 
      {device_properties, device_service:device_properties(Devices)}
    ]}.
