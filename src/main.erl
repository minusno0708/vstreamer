-module(main).
-export([main/1]).

-import(server, [start/1]).

main(_) ->
    server:start(8080).