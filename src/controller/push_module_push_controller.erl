%This controller module is a parameterized module, as indicated by the parameter list ([Req]) in the -module directive. 
%This means that every function will have access to the Req variable, which has a lot of useful information about the current request.
-module(push_module_push_controller, [Req, _SessionId]).
-export([before_/1, new_message/3]).
-include("include/defines.hrl").

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
  {Title, Body, ScheduledFor}  = request_utils:params(["title", "body", "date"], Req),
  [Year, ZeroBasedMonth, Day, Hour, Minute] = lists:flatten(lists:map(fun(Str) -> 
          {Int, _Rest} = string:to_integer(Str), 
          Int 
      end, 
      string:tokens(ScheduledFor, ","))),
  Month = ZeroBasedMonth + 1,
  io:format("Received ~p ~p ~p ~p ~p ~p ~p~n", [Title, Body, Year, Month, Day, Hour, Minute]),
  ScheduledDateTime = {{Year, Month, Day}, {Hour, Minute, 0}},

  case validate_input_list([Title, Body]) of 
    true ->
      % Todo: handle error creating notification_template
      notification_service:create_notification_template(Title, Body, ScheduledDateTime),
      {ok, [{success, "Notification created"}]};
    false ->
      {ok, [{error, "Invalid input"}]}
  end.

%%%
%%% Internal
%%%

% Validates each input
% @param InputList: a list of input
% @return: true if input is OK, else false
validate_input_list(InputList) ->
  validate_input_list(InputList, []).

validate_input_list([], Acc) ->
  lists:member(false, Acc) == false;
validate_input_list([Input|T], Acc) ->
  Result = validate_input(Input),
  validate_input_list(T, [Result|Acc]).

% Returns true if Input is a non-empty string
validate_input(Input) when is_list(Input) ->
  TrimmedString = re:replace(Input, "(\\s+)", "", [global,{return,list}]),
  length(TrimmedString) > 0;
validate_input(_) ->
  false.
















%%%
%%% Temporary / To be removed
%%%

list('GET', []) ->
  Tokens = boss_db:find(device, []),
  {ok, [{tokens, Tokens}]}.


create('GET', []) ->
   ok;
create('POST', []) ->
  Token = request_utils:param("token", Req),
  case device_service:persist_gcm_token(Token) of
    ok -> {redirect, [{action, "list"}]};
    {error, ErrorsList} -> {ok, [{errors, ErrorsList}]} 
  end.

delete('POST', []) ->
  Token = request_utils:param("token_id", Req),
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
