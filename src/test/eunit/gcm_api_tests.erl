-module(gcm_api_tests).
-include_lib("eunit/include/eunit.hrl").

create_single_receiver_request_test() ->
  ApiKey = "apiKey123",
  ReceiverTokens = ["SingleReceiver"],
  MsgTitle = "Title",
  MsgBody = "Body",
  Request = gcm_api:create_request(ApiKey, ReceiverTokens, MsgTitle, MsgBody),

  ExpectedResult = {"https://gcm-http.googleapis.com/gcm/send",
                    [{"Authorization", "key=apiKey123"}], 
                    "application/json",
                    "{\"to\":\"SingleReceiver\",\"priority\":\"high\",\"notification\":{\"sound\":\"default\",\"title\":\"Title\",\"body\":\"Body\"}}"},
  ?assert(Request == ExpectedResult).

create_multiple_receivers_request_test() ->
  ApiKey = "apiKey123",
  ReceiverTokens = ["Rec1", "Rec2", "Rec3"],
  MsgTitle = "MsgTitle",
  MsgBody = "MsgBody",
  Request = gcm_api:create_request(ApiKey, ReceiverTokens, MsgTitle, MsgBody),

  ExpectedResult = {"https://gcm-http.googleapis.com/gcm/send",
                    [{"Authorization", "key=apiKey123"}], 
                    "application/json",
                    "{\"registration_ids\":[\"Rec1\",\"Rec2\",\"Rec3\"],\"priority\":\"high\",\"notification\":{\"sound\":\"default\",\"title\":\"MsgTitle\",\"body\":\"MsgBody\"}}"},
  ?assert(Request == ExpectedResult).


handle_empty_result_response_test() ->
  Response = "{\"multicast_id\":123,\"success\":1,\"failure\":0,\"canonical_ids\":0}",
  Tokens = ["pass"],
  {ok, TokenStatusList} = gcm_api:handle_response(Response, Tokens),

  ExpectedResult = [],
  ?assert(TokenStatusList == ExpectedResult).

handle_successful_result_response_test() ->
  Response = "{\"multicast_id\":123,\"success\":1,\"failure\":0,\"canonical_ids\":0,\"results\":[{\"message_id\":\"0:145713\"}]}",
  Tokens = ["pass"],
  {ok, TokenStatusList} = gcm_api:handle_response(Response, Tokens),

  ExpectedResult = [{"pass", {ok, "0:145713"}}],
  ?assert(TokenStatusList == ExpectedResult).

handle_mixed_result_response_test() ->
  ResponseWithFailures = "{\"multicast_id\":4603,\"success\":2,\"failure\":5,\"canonical_ids\":1,\"results\":[{\"message_id\":\"0:14513\"},{\"error\":\"InvalidRegistration\"},{\"error\":\"Unavailable\"},{\"error\":\"NotRegistered\"},{\"error\":\"InternalServerError\"},{\"error\":\"UnhandledError\"},[{\"message_id\":\"1:2342\"},{\"registration_id\":\"32\"}]]}",

  ExpectedResult = [{"pass1", {ok, "0:14513"}}, {"fail_invalid_registration", remove}, {"fail_unavailable", retry}, {"fail_not_registered", remove}, {"fail_server_error", retry}, {"unhandled_error", error}, {"pass_but_replace", {{ok, "1:2342"}, {new_token, "32"}}}],

  Tokens = ["pass1", "fail_invalid_registration", "fail_unavailable", "fail_not_registered", "fail_server_error", "unhandled_error", "pass_but_replace"],
  {ok, TokenStatusList} = gcm_api:handle_response(ResponseWithFailures, Tokens),
  ?assert(ExpectedResult == TokenStatusList).
