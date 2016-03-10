-module(push_module_01_network).

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
  ssl:start(),
  application:start(inets),
  {ok, []}.

stop(_ListOfWatchIDs) ->
  ssl:stop(),
  application:stop(inets).

