-module(request_utils).
-export([param/2, params/2]).

%%%
%%% API
%%%

% Input: 
% Params = ["body", "title", "user"], 
% Req = Simple bridge request object with contents: {"body":"hejsan", "user":"maggan"}
% Output {"hejsan", [], "maggan"}
params(Params, Req) ->
  params(Params, Req, []).

param(Param, Req) ->
  evaluate_param(Param, Req).


%%% Internal

params([], _Req, Acc) ->
  erlang:list_to_tuple(lists:reverse(Acc));
params([Param|T], Req, Acc) ->
  EvaluatedParam = evaluate_param(Param, Req),
  params(T, Req, [EvaluatedParam|Acc]).


% @param Param = "password"
% @param Req = Simple bridge request object with contents: {"password":"secretpassword123"}
% @return "secretpassword123" | []
evaluate_param(Param, Req) ->
  case json_body_from_request(Req) of

    undefined ->   form_param(Param, Req);

    PropList  ->   
      case proplists:get_value(Param, PropList) of
        undefined   -> [];
        Value       -> Value
      end
  end.

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


form_param(Param, Req) ->
  case Req:post_param(Param) of
    "undefined" -> [];
    Value       -> Value
  end.

