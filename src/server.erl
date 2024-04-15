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
    spawn(fun() -> handle_server(Sock) end),
    loop_acceptor(LSock).    
    
handle_server(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Msg} -> 
            io:format("Received: ~p~n", [Msg]),
            send_resp(Sock),
            handle_server(Sock);
        {error, closed} -> ok
    end.

send_resp(Sock) ->
    Msg = "Hello, client!",
    Headers = 
        [
        "HTTP/1.1 200 OK\r\n",
        "Content-Type: text/plain\r\n",
        "Content-Length: " ++ integer_to_list(length(Msg)) ++ "\r\n",
        "\r\n"
        ],
    Resp = lists:concat(Headers) ++ Msg,
    gen_tcp:send(Sock, Resp).
