%This controller module is a parameterized module, as indicated by the parameter list ([Req]) in the -module directive. 
%This means that every function will have access to the Req variable, which has a lot of useful information about the current request.
-module(push_module_push_controller, [Req, _SessionId]).
-export([before_/1, new_message/3, view_messages/3, view_message/3, delete_message/3, edit_message/3]).
-include("include/defines.hrl").

% Before executing an action, Chicago Boss checks to see if the controller has an before_ function. 
% If so, it passes the action name to the before_ function and checks the return value. 
% If Boss gets a return value of {ok, Credentials}, it proceeds to execute the action, and it passes Credentials as the third argument to the action. 
% If Boss instead gets {redirect, Location}, it redirects the user without executing the action at all. 
% Note that if an action only takes two arguments, the before_ step is skipped altogether.
before_(_) ->
  auth_lib:require_login(Req).


% Gets the new_message page
new_message('GET', [], _Admin) ->
  ok;
new_message('POST', [], Admin) ->
  {Title, Body, ScheduledFor}  = request_utils:params(["title", "body", "date"], Req),
  case parse_notification_template_input(Title, Body, ScheduledFor) of
    {ok, ConvertedScheduleDate} ->
      % Todo: handle error creating notification_template
      notification_service:create_notification_template(Title, Body, ConvertedScheduleDate, Admin:id()),
      {ok, [{success, "Notification created"}]};
    {error, ConvertedScheduleDate} ->
      DummyTemplate = boss_record:new(notification_template, [{title, Title}, {body, Body}, {scheduled_for, ConvertedScheduleDate}]),
      {ok, [{error, "Invalid input"}, {template, DummyTemplate}]}
  end.


view_messages('GET', [], _Admin) ->
  Notifications = notification_service:all_notification_templates(),
  {ok, [{notifications, Notifications}]}.

view_message('GET', [NotificationTemplateId], _Admin) ->
  NotificationTemplate = notification_service:notification_template_with_id(NotificationTemplateId),
  DispatchedNotifications = NotificationTemplate:notification(),
  Devices = lists:flatten(lists:map(fun(Notification) -> Notification:device() end, DispatchedNotifications)),
  Users = lists:map(fun(Device) -> Device:usr() end, Devices),
  {ok, [
      {template, NotificationTemplate}, 
      {user_properties, user_service:user_properties(Users)}
    ]}.

delete_message('DELETE', [NotificationTemplateId], _Admin) ->
  case notification_service:delete_notification_template_with_id(NotificationTemplateId) of
    ok                -> {200, [], []};
    {error, _Reason}  -> {500, [], []}
  end.

edit_message('GET', [NotificationTemplateId], _Admin) ->
  Notification = notification_service:notification_template_with_id(NotificationTemplateId),
  {ok, [{template, Notification}]};
edit_message('POST', [NotificationTemplateId], _Admin) ->
  Notification = notification_service:notification_template_with_id(NotificationTemplateId),
  case Notification:is_pending() of
    true ->
      {Title, Body, ScheduledFor}  = request_utils:params(["title", "body", "date"], Req),
      case parse_notification_template_input(Title, Body, ScheduledFor) of
        {ok, ConvertedScheduleDate} ->
          Properties = [{title, Title},{body, Body},{scheduled_for, ConvertedScheduleDate}],
          UpdatedNotification = notification_service:update_notification_template(Notification, Properties),
          {ok, [{success, "Notification updated"}, {template, UpdatedNotification}]};
        {error, ConvertedScheduleDate} ->
          DummyTemplate = boss_record:new(notification_template, [{title, Title}, {body, Body}, {scheduled_for, ConvertedScheduleDate}]),
          {ok, [{error, "Invalid input"}, {template, DummyTemplate}]}
      end;
    false ->
      {ok, [{error, "Notification has already been sent"}, {template, Notification}]}
  end.

%%%
%%% Internal
%%%

% Parses and validates the notification_template input parameters
% Returns {ok, ParsedScheduleDate} | {error, ParsedScheduleDate} | error
parse_notification_template_input(Title, Body, ScheduledFor) ->
  io:format("Validating ~p ~p ~p~n", [Title, Body, ScheduledFor]),
  try
    % Convert date input components to integers
    [Year, ZeroBasedMonth, Day, Hour, Minute] = lists:flatten(lists:map(fun(Str) -> 
            {Int, _Rest} = string:to_integer(Str), 
            Int 
        end, 
        string:tokens(ScheduledFor, ","))),

    Month = ZeroBasedMonth + 1,
    io:format("Converted input date to ~p ~p ~p ~p ~p ~p ~p~n", [Title, Body, Year, Month, Day, Hour, Minute]),
    ScheduledDateTime = {{Year, Month, Day}, {Hour, Minute, 0}},

    case validate_input_list([Title, Body]) of
      true -> {ok, ScheduledDateTime};
      false -> {error, ScheduledDateTime}
    end

  catch
    _Exception:_Reason -> 
      io:format("Failed converting input date"),
      {error, date_utils:local_datetime()}
  end.

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
