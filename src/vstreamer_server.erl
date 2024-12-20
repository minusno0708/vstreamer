-module(vstreamer_server).

-export([run/1]).

run(Port) ->
    logger:info("Start streaming server on http://localhost:~p~n", [Port]),
    case gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]) of
        {ok, LSock} ->
            loop_acceptor(LSock);
        {error, Reason} ->
            logger:error("Error: ~p~n", [Reason]),
            ok
    end.

loop_acceptor(LSock) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> handle_server(Sock) end),
    loop_acceptor(LSock).

handle_server(Sock) ->
    case read_req(Sock) of
        {ok, [Method, Path, _Version], _ReqHeader, ReqBody} ->
            logger:info("Received: ~p ~p~n", [Method, Path]),
            case vstreamer_router:router(Method, Path, ReqBody) of
                {Status, RespHeader, RespBody} ->
                    send_resp(Sock, Status, RespHeader, RespBody);
                {Status, RespHeader} ->
                    send_resp(Sock, Status, RespHeader)
            end,
            handle_server(Sock);
        {error, closed} -> ok
    end.

read_req(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Req} ->
            {Status, Header, Body} = vstreamer_http:parse_http(Req),
            validate_recv(Sock, Status, Header, Body);
        {error, closed} -> {error, closed}
    end.

continue_recv(Sock, Status, Header, ReceivedBody) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Req} ->
            Body = vstreamer_http:conn_body([Req], ReceivedBody),
            validate_recv(Sock, Status, Header, Body);
        {error, closed} -> {error, closed}
    end.

validate_recv(Sock, Status, Header, Body) ->
    case is_fully_received(Header, Body) of
        true -> {ok, Status, Header, Body};
        false -> continue_recv(Sock, Status, Header, Body)
    end.

is_fully_received(Header, Body) ->
    byte_size(Body) >= binary_to_integer(maps:get(<<"Content-Length">>, Header, <<"0">>)).

send_resp(Sock, Status, Header) ->
    Resp = vstreamer_http:serialize_http(Status, Header),
    gen_tcp:send(Sock, Resp).

send_resp(Sock, Status, Header, Body) ->
    Resp = vstreamer_http:serialize_http(Status, Header, Body),
    gen_tcp:send(Sock, Resp).
