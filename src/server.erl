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
    case read_req(Sock) of
        {ok, States, _Headers, _Body} ->
            io:format("Received: ~p~n", [States]),
            case States of
                [<<"GET">>, <<"/">>, _] ->
                    send_resp(Sock, "200 OK", "Hello, client!");
                _ ->
                    send_resp(Sock, "404 Not Found", "Not found!")
            end,
            handle_server(Sock);
        {error, closed} -> ok
    end.

read_req(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} -> 
            [Header, Body] = string:split(Data, "\r\n\r\n", all),
            [StateField | HeaderField] = string:split(Header, "\r\n", all),
            States = string:split(StateField, " ", all),
            Headers = headers_to_map(HeaderField, #{}),
            {ok, States, Headers, Body};
        {error, closed} -> {error, closed}
    end.

headers_to_map(Headers, Map) ->
    case Headers of
        [] -> Map;
        [Header | Rest] -> 
            [Key, Value] = string:split(Header, ": ", all),
            headers_to_map(Rest, Map#{Key => Value})
    end.

send_resp(Sock, Status, Body) ->
    Headers = 
        [
        "HTTP/1.1 " ++ Status ++ " \r\n",
        "Content-Type: text/plain\r\n",
        "Content-Length: " ++ integer_to_list(length(Body)) ++ "\r\n",
        "\r\n"
        ],
    Resp = lists:concat(Headers) ++ Body,
    gen_tcp:send(Sock, Resp).
