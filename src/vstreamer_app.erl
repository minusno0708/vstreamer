-module(main).
-export([run/0, run/1]).

-import(server, [start/1]).

run() ->
    server:start(8080).

run([Port]) ->
    try
        server:start(list_to_integer(Port))
    catch
        error:Reason -> io:format("Error: ~p~n", [Reason])
    end.