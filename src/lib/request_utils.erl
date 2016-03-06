-module(request_utils).
-export([param_from_json/2, json_body_from_request/1]).

% Returns the received param from the request body, or undefined.
param_from_json(Param, Req) ->
  case json_body_from_request(Req) of
    undefined ->   undefined;
    PropList  ->   proplists:get_value(Param, PropList)
  end.

% Returns the body from the request body as a property list, or undefined.
json_body_from_request(Req) ->
  JsonBody = Req:request_body(),
  try
    {struct, PropList} = mochijson:decode(JsonBody),
    PropList
  catch
    _:_   -> undefined
  end.
