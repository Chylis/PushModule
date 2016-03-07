-module(user_model, [Id, GcmToken, CreatedAt, UpdatedAt]).
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
  ModifiedUser = set([{created_at, Now}, {updated_at, Now}]),
  {ok, ModifiedUser}.

after_create() ->
  boss_mq:push("new-users", THIS).

before_update() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  ModifiedUser = set([{updated_at, Now}]),
  {ok, ModifiedUser}.
