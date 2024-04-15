-module(server).
-export([start/0]).

start() ->
    Port = 8080,
    io:format("Start streaming server on ~p~n", [Port]),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
    {ok, Sock} = gen_tcp:accept(LSock),
    {ok, Bin} = do_recv(Sock, []),
    ok = gen_tcp:close(Sock),
    ok = gen_tcp:close(LSock),
    Bin.
    
do_recv(Sock, Bs) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, B} -> do_recv(Sock, [Bs, B]);
        {error, closed} -> {ok, list_to_binary(Bs)}
    end.
