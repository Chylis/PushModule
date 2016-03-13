-module(push_module_mq).
-export([init/0, stop/1]).

init() ->
  boss_mq:start(),
  {ok, []}.

stop(_ListOfWatchIDs) ->
  boss_mq:stop().
