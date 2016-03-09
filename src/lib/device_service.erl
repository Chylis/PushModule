-module(device_service).
-export([delete_device_with_gcm_token/1, persist_gcm_token/1]).

%%%============================================================================
%%% API
%%%============================================================================

% Persists a new device with the received token. 
% Returns {ok, SavedDevice} | {error, [ErrorMessages]}
persist_gcm_token(Token) ->
  Device = case boss_db:find(device, [{gcm_token, Token}]) of
           []                 -> boss_record:new(device, [{gcm_token, Token}]);
           [ExistingDevice]   -> ExistingDevice:set(gcm_token, Token) % Doesn't really do anything if the token already exists (yet).
         end, 
  Device:save().

% Deletes a token if it exists.
% Returns ok
delete_device_with_gcm_token(Token) ->
  case boss_db:find_first(device, [{gcm_token, Token}]) of
    undefined -> ok;
    Device -> boss_db:delete(Device:id()) 
  end, 
  ok.
