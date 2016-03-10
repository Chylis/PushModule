-module(push_module_auth_controller, [Req, SessionId]).
-compile(export_all).

-define(DEFAULT_REDIRECT, "/push/new_message").
-define(FIRST_PAGE, "/auth/login").

% Inject into login template the key 'redirect' with a value of the URL of the page that the user came from (stored in the 'Referer' HTTP header)
login('GET', []) ->
  {ok, [{redirect, Req:header(referer)}]};

% Verifies name and password and redirects the user if authentication was successful
login('POST', []) ->
  Name = request_utils:param_from_request("name", Req),
  Password = request_utils:param_from_request("password", Req),

  case boss_db:find(admin, [{name, Name}], [{limit,1}]) of
    [Admin] ->
      case Admin:check_password(Password) of
        true ->
          populate_session(SessionId, Admin),
          {redirect, proplists:get_value("redirect", Req:post_params(), ?DEFAULT_REDIRECT), Admin:login_cookies()};
        false ->
          {ok, [{error, "Bad name/password combination"}]}
      end;
    [] ->
      {ok, [{error, "Invalid username: " ++ Name}]}
  end.


logout('GET', []) ->
  delete_session(SessionId),
  {redirect, ?FIRST_PAGE,
    [ mochiweb_cookies:cookie("admin_id", "", [{path, "/"}]),
      mochiweb_cookies:cookie("session_id", "", [{path, "/"}]) ]}.



populate_session(SessionId, Admin) ->
  boss_session:set_session_data(SessionId, admin_name, Admin:name()).

delete_session(SessionId) ->
  boss_session:delete_session(SessionId).
