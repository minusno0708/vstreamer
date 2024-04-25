-module(stream).
-export([load_video/1, send_playlist/2, send_segment/2]).

load_video(FileName) ->
    Path = "../videos/" ++ binary_to_list(FileName), 
    case file:read_file(Path) of
        {ok, File} ->
            {ok, File};
        {error, Reason} ->
            {error, Reason}
    end.

send_playlist(Sock, Playlist) ->
    RawPlaylist = binary_to_list(Playlist),
    Resp =
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(RawPlaylist)) ++ "\r\n",
        "Content-Type: application/dash+xml\r\n",
        "Access-Control-Allow-Origin: *\r\n"
        "\r\n",
        RawPlaylist
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
