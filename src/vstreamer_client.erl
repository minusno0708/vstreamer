-module(vstreamer_client).

-export([send_msg/0]).

send_msg() ->
    Port = 8080,
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {active, false}, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Hello, world!"),
    {ok, Data} = gen_tcp:recv(Sock, 0),
    logger:info("Received: ~p~n", [Data]),
    ok = gen_tcp:close(Sock).
