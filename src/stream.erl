-module(stream).
-export([load_video/1, send_manifest/2, send_segment/2]).


load_video(VideoPath) ->
    Path = "../videos/" ++ binary_to_list(VideoPath), 
    case file:read_file(Path) of
        {ok, File} ->
            case string:split(binary_to_list(VideoPath), ".", all) of
                [_, "mpd"] -> {manifest, File};
                [_, "m4s"] -> {segment, File};
                _ -> {error, "Invalid file type"}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

send_manifest(Sock, Manifest) ->
    io:format("Sending manifest~n"),
    RawManifest = binary_to_list(Manifest),
    Resp =
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(RawManifest)) ++ "\r\n",
        "Content-Type: video/mp4\r\n",
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n",
        RawManifest
        ]),
    gen_tcp:send(Sock, Resp).

send_segment(Sock, Segment) ->
    RawSegment = binary_to_list(Segment),
    Resp =
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(RawSegment)) ++ "\r\n",
        "Content-Type: video/mp4\r\n",
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n",
        RawSegment
        ]),
    gen_tcp:send(Sock, Resp).
