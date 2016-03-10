%This controller module is a parameterized module, as indicated by the parameter list ([Req]) in the -module directive. 
%This means that every function will have access to the Req variable, which has a lot of useful information about the current request.
-module(push_module_push_controller, [Req, _SessionId]).
-compile(export_all).


% Before executing an action, Chicago Boss checks to see if the controller has an before_ function. 
% If so, it passes the action name to the before_ function and checks the return value. 
% If Boss gets a return value of {ok, Credentials}, it proceeds to execute the action, and it passes Credentials as the third argument to the action. 
% If Boss instead gets {redirect, Location}, it redirects the user without executing the action at all. 
% Note that if an action only takes two arguments, the before_ step is skipped altogether.
before_(_) ->
  auth_lib:require_login(Req).


% Gets the new_message page
new_message('GET', [], Admin) ->
  ok;
new_message('POST', [], Admin) ->
  Title = request_utils:param_from_request("title", Req),
  Body = request_utils:param_from_request("body", Req),
  io:format("received ~p ~p~n", [Title, Body]),
  ok.



%%%
%%% To be added
%%%
% -  send POST
% -   














%%%
%%% Temporary / To be removed
%%%

list('GET', []) ->
  Tokens = boss_db:find(device, []),
  {ok, [{tokens, Tokens}]}.


create('GET', []) ->
   ok;
create('POST', []) ->
  Token = request_utils:param_from_request("token", Req),
  case device_service:persist_gcm_token(Token) of
    {ok, _SavedDevice} -> {redirect, [{action, "list"}]};
    {error, ErrorsList} -> {ok, [{errors, ErrorsList}]} 
  end.

delete('POST', []) ->
  Token = request_utils:param_from_request("token_id", Req),
  device_service:delete_device_with_gcm_token(Token),
  {redirect, [{action, "list"}]}.

% Initially called by client retrieve all devices and a timestamp
live('GET', []) ->
  Devices = boss_db:find(device, []),
  Timestamp = boss_mq:now("new-devices"),
  {ok, [{devices, Devices}, {timestamp, Timestamp}]}.

% Called by client on a separate thread, blocks until new devices are available.
% Param timestamp is initially obtained from 'GET live' 
pull('GET', [LastTimestamp]) ->
 {ok, Timestamp, Devices} = boss_mq:pull("new-devices", list_to_integer(LastTimestamp)), % Fetch all new messages since LastTimestamp
 {json, [{timestamp, Timestamp}, {devices, Devices}]}.


send('GET', []) ->
  ServerToken = "AIzaSyCA817K5DPHsfC9NAezrOfKm07KpeiYduw", 
  To = "k6a0XS4-lSY:APA91bEO3D8H3mhYsv51VCs-JQtzCkac-Le7BAP_CtC4huC3nBn05OhZDTo71GFYO8FRQY3hj2V6m_I0Vq43Vjyal-GTPeaDQjY74R5wF84UBgBqAGEK16cA1IQSfcDI1hO3Z2aE1VP4",
  Title = "GCM",
  Body = "Hello from Erlang!",
  gcm_api:send_message(ServerToken, [To], Title, Body),
  {redirect, [{action, "list"}]}.
