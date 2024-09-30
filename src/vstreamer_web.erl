-module(vstreamer_web).

-export([run/1]).

-import(vstreamer_files, [read_page/1, load_video/1, is_exist_video/1, download_video/2, get_video_list/0]).
-import(vstreamer_http, [serialize_http/2, serialize_http/3]).
-import(vstreamer_router, [router/3]).

run(Port) ->
    io:format("Start streaming server on http://localhost:~p~n", [Port]),
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
        {ok, [Method, Path, _Version], _ReqHeaders, _ReqBody} ->
            io:format("Request: ~p ~p~n", [Method, Path]),
            case router(Method, Path, _ReqBody) of
                {Status, Header, File} -> 
                    send_resp(Sock, Status, Header, File);
                {Status, Header} ->
                    send_resp(Sock, Status, Header)
            end,
            handle_server(Sock);
        {error, closed} -> ok
    end.

read_req(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} -> 
            [HeaderSection | BodySection] = string:split(Data, "\r\n\r\n", all),
            [StateLine | HeaderLine] = string:split(HeaderSection, "\r\n", all),

            Status = string:split(StateLine, " ", all),
            Header = headers_to_map(HeaderLine, #{}),
            Body = body_conn(BodySection, <<>>),
            
            case byte_size(Body) >=  binary_to_integer(maps:get(<<"Content-Length">>, Header, <<"0">>)) of
                true -> {ok, Status, Header, Body};
                false -> continue_recv(Sock, Status, Header, Body)
            end;
        {error, closed} -> {error, closed}
    end.

continue_recv(Sock, Status, Header, PreBody) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} ->
            Body = body_conn([Data], PreBody),
            case byte_size(Body) >= binary_to_integer(maps:get(<<"Content-Length">>, Header, <<"0">>)) of
                true -> {ok, Status, Header, Body};
                false -> continue_recv(Sock, Status, Header, Body)
            end;
        {error, closed} -> {error, closed}
    end.

headers_to_map(HeaderList, HeaderMap) ->
    case HeaderList of
        [] -> HeaderMap;
        [Header | Rest] ->
            case string:split(Header, ": ", all) of
                [Key, Value] -> 
                    headers_to_map(Rest, HeaderMap#{Key => Value});
                _ -> 
                    headers_to_map(Rest, HeaderMap)
            end
    end.

body_conn(BodySection, Body) ->
    case BodySection of
        [] -> Body;
        [BodyHead] -> <<Body/binary, BodyHead/binary>>;
        [BodyHead | BodyTail] ->
            body_conn(BodyTail, <<Body/binary, BodyHead/binary, "\r\n\r\n">>)
    end.

send_resp(Sock, Status, Header) ->
    Resp = serialize_http(Status, Header),
    gen_tcp:send(Sock, Resp).

send_resp(Sock, Status, Header, Body) ->
    Resp = serialize_http(Status, Header, Body),
    gen_tcp:send(Sock, Resp).
