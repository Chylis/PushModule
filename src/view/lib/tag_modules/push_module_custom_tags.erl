-module(push_module_custom_tags).
-compile(export_all).

% put custom tags in here, e.g.
%
% reverse(Variables, Options) ->
%     lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template






%%%
%%% User model related tags
%%%

% Called as '{% value_for_user_property user=user property=property %}' from a template view
value_for_user_property(Variables, Options) ->
  User = proplists:get_value(user, Variables),
  Property = proplists:get_value(property, Variables),
  User:value_for_property(Property).

value_for_device_property(Variables, Options) ->
  Device = proplists:get_value(device, Variables),
  Property = proplists:get_value(property, Variables),
  Device:value_for_property(Property).
