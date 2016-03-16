-module(test_device_controller).
-export([start/0]).

% UPDATE TESTS WITH DEVICE TOKEN

% http://chicagoboss.org/doc/api-test.html#boss_assert
% http://www.evanmiller.org/functional-tests-as-a-tree-of-continuations.html
start() ->
  boss_web_test:post_request("/device/register_token", [], [ <<"{\"token\":\"mag\"}">> ], 
    [ 
                              fun boss_assert:http_ok/1,
                              fun(Res) -> 
                                  TokenWasStored = boss_db:find_first(device, [{gcm_token, "mag"}]) =/= undefined,
                                  {TokenWasStored, "Failed to persist token"}
                              end
                             ], 
                             
                             [ %Continuations
                              "Remove the previously registered token",
                              fun(Response) ->
                                boss_web_test:post_request("/device/unregister_token", [], [ <<"{\"token\":\"mag\"}">> ], 
                                  [ 
                                                              fun boss_assert:http_ok/1,
                                                              fun(Res) -> 
                                                                  TokenWasRemoved = boss_db:find_first(device, [{gcm_token, "mag"}]) == undefined,
                                                                  {TokenWasRemoved, "Failed to remove token"}
                                                              end
                                                             ], [])
                              end
                             ]),

  boss_web_test:post_request("/device/register_token", [], [ <<"{\"wrongTokenName\":\"mag\"}">> ], [ fun boss_assert:http_bad_request/1 ], []),

  boss_web_test:post_request("/device/register_token/wrongPath", [], [ <<"{\"token\":\"mag\"}">> ], [ fun boss_assert:http_not_found/1 ], []),

  boss_web_test:post_request("/device/unregister_token", [], [ <<"{\"wrongTokenName\":\"mag\"}">> ], [ fun boss_assert:http_bad_request/1 ], []),

  boss_web_test:post_request("/device/unregister_token/wrongPath", [], [ <<"{\"token\":\"mag\"}">> ], [ fun boss_assert:http_not_found/1 ], []).
