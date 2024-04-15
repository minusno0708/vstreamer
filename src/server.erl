-module(server).
-export([start/0]).

start() ->
    Port = 8080,
    io:format("Start streaming server on ~p~n", [Port]),
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]) of
        {ok, LSock} -> 
            loop_acceptor(LSock),
            gen_tcp:close(LSock);
        {error, Reason} -> 
            io:format("Error: ~p~n", [Reason]),
            ok
    end.

loop_acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> handle_server(Sock) end),
    loop_acceptor(LSock).

handle_server(Sock) ->
    {ok, Bin} = do_recv(Sock, []),
    io:format("Received: ~p~n", [Bin]),
    ok = gen_tcp:close(Sock).
    
do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} -> do_recv(Sock, [Bs, B]);
        {error, closed} -> {ok, list_to_binary(Bs)}
    end.
