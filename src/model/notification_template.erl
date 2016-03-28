-module(notification_template, [
    Id, 
    AdminId,      % Id of admin that created notification
    Status::string(),       % Status string, 'Sent' | 'Sending' | 'Pending' | 'Failed: Retry' | 'Error: <Reason>'
    Title::string(),        % Title of notification
    Body::string(),         % Body of notification
    ScheduledFor::datetime(), % Schedule date
    SentAt::datetime(),       % Date notification was sent
    CreatedAt::datetime(),    % Creation date
    UpdatedAt::datetime()     % Update date
  ]).
-compile(export_all).
-belongs_to(admin).
-has({notification, many}).

%%%============================================================================
%%% API
%%%============================================================================

formatted_created_at() ->
  date_utils:format_datetime(CreatedAt).

formatted_updated_at() ->
  date_utils:format_datetime(UpdatedAt).

formatted_scheduled_for() ->
  date_utils:format_datetime(ScheduledFor).

formatted_sent_at() ->
  date_utils:format_datetime(SentAt).

is_sent() ->
  Status == "Sent".

is_sending() ->
  Status == "Sending".

is_pending() ->
  Status == "Pending".

%%%============================================================================
%%% Validation 
%%%============================================================================

validation_tests() ->
  [
    { fun() -> is_list(Title) andalso length(Title) > 0 end, {title, "Title Required"} },
    { fun() -> is_list(Body) andalso length(Body) > 0   end, {body, "Body Required"}   },
    { fun() -> ScheduledFor =/= undefined  end, {scheduled_for, "Notification templates must have a schedule date"}   }
  ].

%%%============================================================================
%%% Hooks 
%%%============================================================================

before_create() ->
  Now = date_utils:local_datetime(),
  Modified = set([{sent_messages, []}, {created_at, Now}, {updated_at, Now}, {status, "Pending"}]),
  {ok, Modified}.

before_update() ->
  Now = date_utils:local_datetime(),
  Modified = set([{updated_at, Now}]),
  {ok, Modified}.
