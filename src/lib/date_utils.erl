-module(date_utils).
-export([local_datetime/0, format_datetime/1, format_utc_timestamp/0]).

local_datetime() ->
  calendar:local_time().

format_datetime({{Year,Month,Day},{Hour,Minute,_Second}}) ->
  Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"}),
  io_lib:format("~2w ~s ~4w ~2w:~2..0w", [Day,Mstr,Year,Hour,Minute]).

format_utc_timestamp() ->
  TS = {_,_,Micro} = os:timestamp(),
  {{Year,Month,Day},{Hour,Minute,Second}} = calendar:now_to_local_time(TS),
  Mstr = element(Month,{"Jan","Feb","Mar","Apr","May","Jun","Jul",
      "Aug","Sep","Oct","Nov","Dec"}),
  io_lib:format("~2w ~s ~4w ~2w:~2..0w:~2..0w.~6..0w",
    [Day,Mstr,Year,Hour,Minute,Second,Micro]).
