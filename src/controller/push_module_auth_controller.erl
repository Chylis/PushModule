-module(push_module_auth_controller, [Req, SessionId]).
-export([login/2, logout/2]).

-define(DEFAULT_REDIRECT, "/push/new_message").
-define(FIRST_PAGE, "/auth/login").

% Inject into login template the key 'redirect' with a value of the URL of the page that the user came from (stored in the 'Referer' HTTP header)
login('GET', []) ->
  {ok, [{redirect, Req:header(referer)}]};

% Verifies name and password and redirects the user if authentication was successful
login('POST', []) ->
  {Name, Password} = request_utils:params(["name", "password"], Req),
  case boss_db:find(admin, [{name, Name}], [{limit,1}]) of
    [Admin] ->
      case Admin:check_password(Password) of
        true ->
          session_handler:populate_session(SessionId, Admin),
          {redirect, proplists:get_value("redirect", Req:post_params(), ?DEFAULT_REDIRECT), Admin:login_cookies()};
        false ->
          {ok, [{error, "Bad name/password combination"}]}
      end;
    [] ->
      {ok, [{error, "Invalid username: " ++ Name}]}
  end.


logout('GET', []) ->
  session_handler:delete_session(SessionId),
  {redirect, ?FIRST_PAGE, auth_lib:logout_cookies()}.
