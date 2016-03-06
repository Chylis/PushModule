-module(user_model_tests).
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% API
%%%============================================================================

suite_test_()->
  Suite = {foreach, local, fun setup/0, tests()},
  Suite.

tests() ->
  [       
   {"Validations of gcm token",       ?_test(val_gcm_token())},
   {"Validations of generated dates", ?_test(val_gen_dates())}
  ].

val_gcm_token() ->
  User = boss_record:new(user_model, []),
  ?assertEqual({error, [{gcm_token, "Required"}]}, User:save()),

  User2 = User:set([{gcm_token, "token123"}]),
  {Res, _} = User2:save(),
  ?assertEqual(ok, Res).

val_gen_dates() ->
  User = boss_record:new(user_model, [{gcm_token, "token123"}]),
  {ok, SavedUser} = User:save(),
  ?assertNotEqual(undefined, SavedUser:created_at()),
  ?assertNotEqual(undefined, SavedUser:updated_at()).


%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------
setup()->
  ok.
