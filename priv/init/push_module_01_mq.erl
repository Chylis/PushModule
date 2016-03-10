-module(push_module_01_mq).

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
  boss_mq:start(),
  {ok, []}.

stop(_ListOfWatchIDs) ->
  boss_mq:stop().

