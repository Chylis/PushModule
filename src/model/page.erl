-module(page, [Id, ViewId, PageDataJson]).
-compile(export_all).

validation_tests() ->
    [{fun() -> length(PageDataJson) > 0 end, "Page Data cannot be empty"}].
