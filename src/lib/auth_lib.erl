-module(auth_lib).
-compile(export_all).

hash_for(Name, Password) ->
  MD5 = erlang:md5(Name),
  Salt = mochihex:to_hex(MD5),
  hash_password(Password, Salt).

hash_password(Password, Salt) ->
  MD5 = erlang:md5(Salt ++ Password),
  mochihex:to_hex(MD5).


% Called as a 'before' action by controllers that require authentication.
% Checks if the 'admin_id' cookie has is present, and if so, fetches the admin (if it exists) and compares its session_identifier with the current session_id.
% Returns {ok, Admin} or redirects the user to the login page
require_login(Req) ->
  RedirectAction = {redirect, "/auth/login"},

  case Req:cookie("admin_id") of
    undefined -> RedirectAction;
    Id ->
      case boss_db:find(Id) of
        undefined -> RedirectAction;
        Admin ->
          case Admin:session_identifier() =:= Req:cookie("session_id") of
            false -> RedirectAction;
            true -> {ok, Admin}
          end
      end
  end.
