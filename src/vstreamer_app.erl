-module(vstreamer_app).

-export([start/0, start/1]).

-import(vstreamer_web, [run/1]).

start() ->
    start(8080).

start(Port) ->
    try
        run(Port)
    catch
        error:Reason -> io:format("Error: ~p~n", [Reason])
    end.
