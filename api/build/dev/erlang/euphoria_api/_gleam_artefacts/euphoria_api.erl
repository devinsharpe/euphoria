-module(euphoria_api).
-compile([no_auto_import, nowarn_unused_vars, nowarn_unused_function, nowarn_nomatch]).

-export([main/0]).

-file("/Users/devsharpe/Projects/euphoria/api/src/euphoria_api.gleam", 3).
-spec main() -> nil.
main() ->
    gleam@io:println(<<"Hello from euphoria_api!"/utf8>>).
