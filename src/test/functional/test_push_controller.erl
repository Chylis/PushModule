-module(test_push_controller).
-export([start/0]).

start() ->
  boss_web_test:get_request("/push/new_message", [], [ fun boss_assert:http_ok/1 ], []),
  boss_web_test:post_request("push/new_message", [], [<<"{\"title\": \"hej\", \"body\":\"kropp\"}">>], [ fun boss_assert:http_ok/1 ], []).
