-module(push_module_gcm_controller, [Req]).
-export([registerToken/2, unregisterToken/2]).

%%%============================================================================
%%% API
%%%============================================================================

% curl -v http://104.155.38.181:8001/gcm/registerToken -d {\"token\":\"mag\"}
registerToken('POST', []) -> 
  case request_utils:param("token", Req) of
    [] -> {400, ["Missing token"], []};
    Token     ->
      case device_service:persist_gcm_token(Token) of
        {ok, _}                     -> {200, [], []};
        {error, ErrorMessages}      -> {500, ErrorMessages, []}
      end
  end;
registerToken(_, _) ->
  {404, ["Endpoint only accepts POST and doesn't accept any path params"], []}.


unregisterToken('POST', []) ->
  case request_utils:param("token", Req) of
    [] -> {400, ["Missing token"], []};
    Token     -> 
      device_service:delete_device_with_gcm_token(Token),
      {200, [], []}
  end;
unregisterToken(_,_) ->
  {404, ["Endpoint only accepts POST and doesn't accept any path params"], []}.


