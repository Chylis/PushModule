-module(request_utils).
-export([param/2, params/2, property_list_from_json/1]).

%%%
%%% API
%%%

% Fetches several params from a json object
params(Params, Json) when is_list(Json) ->
  json_params(Params, Json, []);

% Fetches several params from a request object
params(Params, Req) ->
  req_params(Params, Req, []).

% Fetches a single param from a json object
param(Param, Json) when is_list(Json) ->
  param_from_json(Param, Json);

% Fetches a single param from a request object
param(Param, Req) ->
  param_from_request(Param, Req).





%%%
%%% Request
%%%

req_params([], _Req, Acc) ->
  erlang:list_to_tuple(lists:reverse(Acc));
req_params([Param|T], Req, Acc) ->
  EvaluatedParam = param_from_request(Param, Req),
  req_params(T, Req, [EvaluatedParam|Acc]).


% Req -> Plist -> Value | 
% Req -> Body -> Value
param_from_request(Param, Req) ->
  case property_list_from_request(Req) of
    undefined ->   form_param_from_request(Param, Req);
    PropList  ->   param_from_property_list(Param, PropList)
  end.

% Req -> Plist
property_list_from_request(Req) ->
  JsonBody = Req:request_body(),
  property_list_from_json(JsonBody).

% Req -> Value
form_param_from_request(Param, Req) ->
  case Req:post_param(Param) of
    "undefined" -> [];
    Value       -> Value
  end.







%%%
%%% JSON 
%%%

json_params([], _Json, Acc) ->
  erlang:list_to_tuple(lists:reverse(Acc));
json_params([Param|T], Json, Acc) ->
  EvaluatedParam = param_from_json(Param, Json),
  json_params(T, Json, [EvaluatedParam|Acc]).

% Json -> PList -> Property | []
param_from_json(Param, Json) ->
  case property_list_from_json(Json) of
    undefined -> [];
    PropList -> param_from_property_list(Param, PropList)
  end.

property_list_from_json(Json) ->
  try
    {struct, PropList} = mochijson:decode(Json),
    PropList
  catch
    _:_   -> undefined
  end.

% Plist -> Property
param_from_property_list(Param, PropList) ->
  case proplists:get_value(Param, PropList) of
    undefined   -> [];
    Value       -> Value
  end.
