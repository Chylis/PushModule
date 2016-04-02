-module(push_module_notification_scheduler).
-export([init/0, stop/1]).

init() ->
  SchedulerId = spawn(notification_scheduler, check_expired_and_unsent_notifications, []),
  {ok, [SchedulerId]}.

stop(Ids) ->
  lists:foreach(fun(Id) -> exit(Id, kill) end, Ids).
