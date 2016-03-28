-module(list_utils).
-compile(export_all).

n_length_chunks(List,Len) ->
  LeaderLength = case length(List) rem Len of
    0 -> 0;
    N -> Len - N
  end,
  Leader = lists:duplicate(LeaderLength,undefined),
  n_length_chunks(Leader ++ lists:reverse(List),[],0,Len).

n_length_chunks([],Acc,_,_) -> Acc;
n_length_chunks([H|T],Acc,Pos,Max) when Pos==Max ->
  n_length_chunks(T,[[H] | Acc],1,Max);
n_length_chunks([H|T],[HAcc | TAcc],Pos,Max) ->
  n_length_chunks(T,[[H | HAcc] | TAcc],Pos+1,Max);
n_length_chunks([H|T],[],Pos,Max) ->
  n_length_chunks(T,[[H]],Pos+1,Max).
