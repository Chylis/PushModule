%This controller module is a parameterized module, as indicated by the parameter list ([Req]) in the -module directive. 
%This means that every function will have access to the Req variable, which has a lot of useful information about the current request.
-module(push_module_sample_controller, [Req]).
-compile(export_all).





%Each action (i.e. 'hello') will have its own URL of the form /<controller name>/<action name>. 
%If the URL contains additional slash-separated tokens beyond the action name, these will be passed as a list to
%the controller action in the second argument.

%Controller actions can return several values. The simplest is {output, Value}, and it returns raw HTML. 
%We can also use {json, Values} to return JSON: {json, [{greeting, "Hello, world!"}]}.
hello('GET', []) ->
  ServerToken = "AIzaSyCA817K5DPHsfC9NAezrOfKm07KpeiYduw", 
  To = "k6a0XS4-lSY:APA91bEO3D8H3mhYsv51VCs-JQtzCkac-Le7BAP_CtC4huC3nBn05OhZDTo71GFYO8FRQY3hj2V6m_I0Vq43Vjyal-GTPeaDQjY74R5wF84UBgBqAGEK16cA1IQSfcDI1hO3Z2aE1VP4",
  Title = "GCM",
  Body = "Hello from Erlang!",
  gcm_api:send_message(ServerToken, [To], Title, Body),
  {output, "<strong>Mag says hello!</strong>"}. 

