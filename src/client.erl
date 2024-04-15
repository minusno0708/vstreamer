-module(client).
-export([send_msg/0]).

send_msg() ->
    Port = 8080,
    {ok, Sock} = gen_tcp:connect("localhost", Port, [binary, {packet, 0}]),
    ok = gen_tcp:send(Sock, "Hello, world!"),
    ok = gen_tcp:close(Sock).