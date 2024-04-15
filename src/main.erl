-module(main).
-export([main/1]).

-import(server, [start/1]).

main([Port]) ->
    try
        start(list_to_integer(Port))
    catch
        error:Reason -> io:format("Error: ~p~n", [Reason])
    end;

main(_) ->
    server:start(8080).
