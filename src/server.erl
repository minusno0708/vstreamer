-module(server).
-export([start/0]).

start() ->
    Port = 8080,
    io:format("Start streaming server on ~p~n", [Port]),
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, LSock} -> 
            loop_acceptor(LSock);
        {error, Reason} -> 
            io:format("Error: ~p~n", [Reason]),
            ok
    end.

loop_acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> do_recv(Sock) end),
    loop_acceptor(LSock).
    
do_recv(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} -> 
            io:format("Received: ~p~n", [Data]),
            gen_tcp:send(Sock, Data),
            do_recv(Sock);
        {error, closed} -> ok
    end.
