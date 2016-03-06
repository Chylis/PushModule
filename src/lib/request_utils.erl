-module(request_utils).
-export([param_from_request/2]).

%%%
%%% API
%%%

% Receives a String param, and a Simple Bridge request object.
% Returns the received param from the request body, or undefined.
param_from_request(Param, Req) ->
  case json_body_from_request(Req) of
    undefined ->   undefined;
    PropList  ->   proplists:get_value(Param, PropList)
  end.

%%%
%%% Internal
%%%

% Receives a Simple Bridge request object.
% Returns the body from the request body as a property list, or undefined.
json_body_from_request(Req) ->
  JsonBody = Req:request_body(),
  try
    {struct, PropList} = mochijson:decode(JsonBody),
    PropList
  catch
    _:_   -> undefined
  end.
