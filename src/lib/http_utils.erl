-module(http_utils).
-export([send_http_request/4]).

%%%
%%% API
%%%

send_http_request(Method, Request, HttpOptions, Options) ->
  io:format("~nSending request: '~p','~p','~p','~p'~n", [Method, Request, HttpOptions, Options]),
  try httpc:request(Method, Request, HttpOptions, Options) of

    {ok, {{_HttpVersion, StatusCode, StatusMessage}, ResponseHeaders, ResponseBody}}   -> 
      io:format("~nReceived response: '~p','~p','~p','~p'~n", [StatusCode, StatusMessage, ResponseHeaders, ResponseBody]),
      {ok, {StatusCode, ResponseHeaders, ResponseBody}};

    {ok, {StatusCode, ResponseBody}} -> 
      io:format("~nReceived response: '~p','~p'~n", [StatusCode, ResponseBody]),
      {ok, {StatusCode, [], ResponseBody}};

    {ok, saved_to_file} -> 
      io:format("~nReceived response: 'saved to file'~n"),
      {ok, saved_to_file};

    {ok, RequestId} -> 
      io:format("~nReceived response requestId: '~p'~n", [RequestId]),
      {ok, RequestId};

    {error, Reason} -> 
      io:format("~nReceived error response: '~p'~n", [Reason]),
      {error, Reason}

  catch Exception -> 
    io:format("~nCaught exception: '~p'~n", [Exception]),
    {error, Exception}
  end.
