-module(user_service).
-export([delete_gcm_token/1, persist_gcm_token/1]).

%%%============================================================================
%%% API
%%%============================================================================

% Persists a new user_model with the received token. 
% Returns {ok, SavedUser} | {error, [ErrorMessages]}
persist_gcm_token(Token) ->
  User = case boss_db:find(user_model, [{gcm_token, Token}]) of
           []               -> boss_record:new(user_model, [{gcm_token, Token}]);
           [ExistingUser]   -> ExistingUser:set(gcm_token, Token) % Doesn't really do anything if the token already exists (yet).
         end, 
  User:save().

% Deletes a token if it exists.
% Returns ok
delete_gcm_token(Token) ->
  case boss_db:find_first(user_model, [{gcm_token, Token}]) of
    undefined -> ok;
    User      -> boss_db:delete(User:id()) 
  end, 
  ok.
