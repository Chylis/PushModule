-module(device_tests).
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
  Device = boss_record:new(device, []),
  ?assertEqual({error, [{gcm_token, "Required"}]}, Device:save()),

  Device2 = Device:set([{gcm_token, "token123"}]),
  {Res, _} = Device2:save(),
  ?assertEqual(ok, Res).

val_gen_dates() ->
  Device = boss_record:new(device, [{gcm_token, "token123"}]),
  {ok, SavedDevice} = Device:save(),
  ?assertNotEqual(undefined, SavedDevice:created_at()),
  ?assertNotEqual(undefined, SavedDevice:updated_at()).


%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------
setup()->
  ok.
