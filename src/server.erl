-module(server).
-export([start/1]).

-import(files, [read_page/1, load_video/1, is_exist_video/1]).

start(Port) ->
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
                    Header = [
                        "Content-Type: text/plain\r\n"
                    ],
                    send_resp(Sock, "200 OK", Header, "Hello, client!");
                [<<"GET">>, <<"/page">>, _] ->
                    {ok, File} = read_page(<<"index">>),
                    Header = [
                        "Content-Type: text/html\r\n"
                    ],
                    send_resp(Sock, "200 OK", Header, File);
                [<<"GET">>, <<"/page/", VideoName/binary>>, _] ->
                    case is_exist_video(VideoName) of
                        true ->
                            {ok, File} = read_page(<<"video">>),
                            Header = [
                                "Content-Type: text/html\r\n"
                            ],
                            send_resp(Sock, "200 OK", Header, 
                                re:replace(File, "%%VIDEO_NAME%%", binary_to_list(VideoName), [{return, list}]));
                        false ->
                            {ok, File} = read_page(<<"404">>),
                            Header = [
                                "Content-Type: text/html\r\n"
                            ],
                            send_resp(Sock, "404 Not Found", Header, File)
                    end;
                [<<"GET">>, <<"/video/", VideoPath/binary>>, _] ->
                    case load_video(VideoPath) of
                        {manifest, File} ->
                            Header = [
                                "Content-Type: video/mp4\r\n",
                                "Access-Control-Allow-Origin: *\r\n"
                            ],
                            send_resp(Sock, "200 OK", Header, binary_to_list(File));
                        {segment, File} ->
                            Header = [
                                "Content-Type: video/mp4\r\n",
                                "Access-Control-Allow-Origin: *\r\n"
                            ],
                            send_resp(Sock, "200 OK", Header, binary_to_list(File));
                        {error, _} ->
                            Header = [
                                "Content-Type: text/plain\r\n"
                            ],
                            send_resp(Sock, "404 Not Found", Header,"Not found!")
                    end;
                _ ->
                    Header = [
                        "Content-Type: text/plain\r\n"
                    ],
                    send_resp(Sock, "404 Not Found", Header,"Not found!")
            end,
            handle_server(Sock);
        {error, closed} -> ok
    end.

read_req(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {ok, Data} -> 
            [Header, Body] = case string:split(Data, "\r\n\r\n", all) of
                [H, B] -> [H, B];
                [H] -> [H, <<"">>]
            end,
            [StateField | HeaderField] = string:split(Header, "\r\n", all),
            States = string:split(StateField, " ", all),
            Headers = headers_to_map(HeaderField, #{}),
            {ok, States, Headers, Body};
        {error, closed} -> {error, closed}
    end.

headers_to_map(HeaderList, HeaderMap) ->
    case HeaderList of
        [] -> HeaderMap;
        [Header | Rest] -> 
            [Key, Value] = string:split(Header, ": ", all),
            headers_to_map(Rest, HeaderMap#{Key => Value})
    end.

send_resp(Sock, Status, Header, Body) ->
    Resp = 
        lists:concat([
        "HTTP/1.1 " ++ Status ++ " \r\n",
        "Content-Length: " ++ integer_to_list(length(Body)) ++ "\r\n",
        Header,
        "\r\n",
        Body
        ]),
    gen_tcp:send(Sock, Resp).
