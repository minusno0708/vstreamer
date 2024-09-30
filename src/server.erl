-module(server).
-export([start/1]).

-import(files, [read_page/1, load_video/1, is_exist_video/1, download_video/2, get_video_list/0]).
-import(vstreamer_http, [serialize_http/2, serialize_http/3]).

start(Port) ->
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
        {ok, States, _Headers, _Body} ->
            io:format("Received: ~p~n", [States]),
            case States of
                [<<"GET">>, <<"/">>, _] ->
                    send_resp(Sock, 302, <<"Location: /page\r\n">>);
                [<<"GET">>, <<"/page">>, _] ->
                    {ok, File} = read_page(<<"index">>),
                    Header = <<"Content-Type: text/html\r\n">>,
                    send_resp(Sock, 200, Header, File);
                [<<"GET">>, <<"/page/list">>, _] ->
                    {ok, File} = read_page(<<"list">>),
                    Header = <<"Content-Type: text/html\r\n">>,
                    EmbedFile = re:replace(binary_to_list(File), "%%VIDEO_LIST%%", get_video_list(), [{return, list}]),
                    send_resp(Sock, 200, Header, list_to_binary(EmbedFile));
                [<<"GET">>, <<"/page/", PageName/binary>>, _] ->
                    case read_page(PageName) of
                        {ok, File} ->
                            Header = <<"Content-Type: text/html\r\n">>,
                            send_resp(Sock, 200, Header, File);
                        {error, File} ->
                            Header = <<"Content-Type: text/html\r\n">>,
                            send_resp(Sock, 404, Header, File)
                    end;
                [<<"GET">>, <<"/video/", VideoName/binary>>, _] ->
                    case is_exist_video(VideoName) of
                        true ->
                            {ok, File} = read_page(<<"video">>),
                            Header = <<"Content-Type: text/html\r\n">>,
                            EmbedFile = re:replace(binary_to_list(File), "%%VIDEO_NAME%%", binary_to_list(VideoName), [{return, list}]),
                            send_resp(Sock, 200, Header, list_to_binary(EmbedFile));
                        false ->
                            {ok, File} = read_page(<<"404">>),
                            Header = <<"Content-Type: text/html\r\n">>,
                            send_resp(Sock, 404, Header, File)
                    end;
                [<<"GET">>, <<"/stream/", VideoPath/binary>>, _] ->
                    case load_video(VideoPath) of
                        {manifest, File} ->
                            Header = <<
                                "Content-Type: video/mp4\r\n",
                                "Access-Control-Allow-Origin: *\r\n"
                            >>,
                            send_resp(Sock, 200, Header, File);
                        {segment, File} ->
                            Header = <<
                                "Content-Type: video/mp4\r\n",
                                "Access-Control-Allow-Origin: *\r\n"
                            >>,
                            send_resp(Sock, 200, Header, File);
                        {error, _} ->
                            Header = <<"Content-Type: text/plain\r\n">>,
                            send_resp(Sock, 404, Header, <<"Not found!">>)
                    end;
                [<<"POST">>, <<"/upload">>, _] ->
                    case extract_video(_Body) of
                        {ok, VideoName, ExtractVideo} ->
                            spawn(fun() -> download_video(VideoName, ExtractVideo) end),
                            Header = <<"Content-Type: text/plain\r\n">>,
                            send_resp(Sock, 201, Header, <<"Upload page">>);
                        error ->
                            Header = <<"Content-Type: text/plain\r\n">>,
                            send_resp(Sock, 500, Header, <<"Failed to update video">>)
                    end;
                _ ->
                    Header = <<"Content-Type: text/plain\r\n">>,
                    send_resp(Sock, 404, Header, <<"Not found!">>)
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

extract_video(Body) ->
    [Header, Video] = string:split(Body, "\r\n\r\n"),
    [Delim | _ ] = string:split(Header, "\r\n", all),
    case maps:find("filename", 
        headers_to_map(string:split(
        repl_mult_words(binary_to_list(Header), [{binary_to_list(Delim), ""}, {"; ", "\r\n"}, {"=", ": "}, {"\"", ""}]),
        "\r\n", all), #{})
    ) of
        {ok, VideoName} ->
            [ExtractVideo, _] = string:split(Video, <<"\r\n", Delim/binary>>),
            {ok, VideoName, ExtractVideo};
        error -> error
    end.

repl_mult_words(Text, Replacements) ->
    case Replacements of
        [] -> Text;
        [{Old, New} | Rest] -> repl_mult_words(string:replace(Text, Old, New, all), Rest)
    end.

send_resp(Sock, Status, Header) ->
    Resp = serialize_http(Status, Header),
    gen_tcp:send(Sock, Resp).

send_resp(Sock, Status, Header, Body) ->
    Resp = serialize_http(Status, Header, Body),
    gen_tcp:send(Sock, Resp).
