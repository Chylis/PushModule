-module(http_utils).
-export([send_http_request/4]).

send_http_request(Method, Request, HttpOptions, Options) ->
  ssl:start(),
  application:start(inets),

  try httpc:request(Method, Request, HttpOptions, Options) of
    {ok, {{_HttpVersion, StatusCode, _ReasonPhrase}, _Headers, ResponseBody}}   -> {ok, {StatusCode, ResponseBody}};
    {ok, {StatusCode, ResponseBody}}                                            -> {ok, {StatusCode, ResponseBody}};
    {ok, saved_to_file}                                                         -> {ok, saved_to_file};
    {ok, RequestId}                                                             -> {ok, RequestId};
    {error, Reason}                                                             -> {error, Reason}
  catch
    Exception                                                                   -> {error, Exception}
  end.
