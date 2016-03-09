-module(device, [Id, GcmToken, CreatedAt, UpdatedAt]).
-compile(export_all).

%%%============================================================================
%%% API
%%%============================================================================

%%%============================================================================
%%% Validation 
%%%============================================================================

validation_tests() ->
  [
   { fun() -> is_list(GcmToken) andalso length(GcmToken) > 0 end,  {gcm_token, "Required"} }
  ].

%%%============================================================================
%%% Hooks 
%%%============================================================================

before_create() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  ModifiedDevice= set([{created_at, Now}, {updated_at, Now}]),
  {ok, ModifiedDevice}.

after_create() ->
  boss_mq:push("new-devices", THIS).

before_update() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  ModifiedDevice= set([{updated_at, Now}]),
  {ok, ModifiedDevice}.
