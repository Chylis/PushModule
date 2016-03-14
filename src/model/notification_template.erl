-module(notification_template, [
    Id, 
    Title,        % Title of notification
    Body,         % Body of notification
    ScheduledFor, % Schedule date
    SentAt,       % Date notification was sent
    Status,       % Status string
    CreatedAt,    % Creation date
    AdminId,      % Id of admin that created notification
    UpdatedAt     % Update date
  ]).
-compile(export_all).
-belongs_to(admin).
-has({notification, many}).

%%%============================================================================
%%% API
%%%============================================================================

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
  Now = calendar:now_to_universal_time(erlang:now()),
  Modified = set([{sent_messages, []}, {created_at, Now}, {updated_at, Now}, {status, "Pending"}]),
  {ok, Modified}.

before_update() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  Modified = set([{updated_at, Now}]),
  {ok, Modified}.
