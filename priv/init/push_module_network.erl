-module(push_module_network).
-export([init/0, stop/1]).

init() ->
  ssl:start(),
  application:start(inets),
  {ok, []}.

stop(_ListOfWatchIDs) ->
  ssl:stop(),
  application:stop(inets).
