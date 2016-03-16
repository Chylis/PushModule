-module(push_module_device_controller, [Req]).
-export([register_device/2, register_token/2, unregister_token/2]).

%%%============================================================================
%%% API
%%%============================================================================

register_device('POST', []) ->
  {ExternalUserId, DeviceId, AdditionalInfoJson}  = request_utils:params(["userId", "deviceId", "additionalInfo"], Req),
  case device_service:create_device(ExternalUserId, DeviceId, AdditionalInfoJson) of
    ok                      -> {200, [], []};
    {error, Reason}         -> {400, Reason, []}
  end.

register_token('POST', []) -> 
  {Token, DeviceId} = request_utils:params(["token", "deviceId"], Req),
    case device_service:persist_gcm_token(Token, DeviceId) of
      ok                      -> {200, [], []};
      {error, Reason}         -> {400, Reason, []}
    end;
register_token(_, _) ->
  {404, ["Endpoint only accepts POST and doesn't accept any path params"], []}.


unregister_token('POST', []) ->
  {Token, DeviceId} = request_utils:params(["token", "deviceId"], Req),
  device_service:delete_gcm_token(Token, DeviceId),
  {200, [], []};
unregister_token(_,_) ->
  {404, ["Endpoint only accepts POST and doesn't accept any path params"], []}.


