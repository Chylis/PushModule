-module(admin, [Id, Name, PasswordHash]).
-compile(export_all).
% API

% When a log-in is successful, we’ll put that session identifier along with the admin Id into a cookie, 
% and if we ever receive that same cookie back, we’ll know it’s the same person who logged in. 
check_password(Password) ->
  MD5 = erlang:md5(Name),
  Salt = mochihex:to_hex(MD5),
  auth_lib:hash_password(Password, Salt) =:= PasswordHash.

login_cookies() ->
  auth_lib:login_cookies(Id, session_identifier()).

% Internal

-define(SECRET_STRING, "Such a secret hashing algorithm").
session_identifier() ->
  mochihex:to_hex(erlang:md5(?SECRET_STRING ++ Id)).
