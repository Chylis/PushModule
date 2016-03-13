-module(device, [
    Id, 
    GcmToken,       % Current token, string
    PreviousTokens, % List of previous gcm tokens,
    IsRemoved,      % If the deivce/token is marked as removed or not, bool
    CreatedAt,      % Date 
    UpdatedAt       % Date
  ]).
-compile(export_all).
-has({notification, many}).

%%%============================================================================
%%% Validation 
%%%============================================================================

validation_tests() ->
  [
   { fun() -> is_list(GcmToken) andalso length(GcmToken) > 0 end,  {gcm_token, "Token Required"} }
  ].

%%%============================================================================
%%% Hooks 
%%%============================================================================

before_create() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  ModifiedDevice = set([{is_removed, false}, {previous_tokens, []}, {created_at, Now}, {updated_at, Now}]),
  {ok, ModifiedDevice}.

before_update() ->
  Now = calendar:now_to_universal_time(erlang:now()),
  ModifiedDevice= set([{updated_at, Now}]),
  {ok, ModifiedDevice}.
