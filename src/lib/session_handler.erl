-module(session_handler).
-export([populate_session/2, delete_session/1]).
-include("include/defines.hrl").

populate_session(SessionId, Admin) ->
  boss_session:set_session_data(SessionId, ?SESSION_KEY_ADMIN_NAME, Admin:name()).

delete_session(SessionId) ->
  boss_session:delete_session(SessionId).
