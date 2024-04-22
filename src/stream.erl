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
    RowPlaylist = binary_to_list(Playlist),
    Resp = 
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(RowPlaylist)) ++ "\r\n",
        "Content-Type: video/mp2t\r\n",
        "\r\n",
        RowPlaylist
        ]),
    gen_tcp:send(Sock, Resp).

send_segment(Sock, Segment) ->
    RowSegment = binary_to_list(Segment),
    Resp = 
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(RowSegment)) ++ "\r\n",
        "Content-Type: video/mp2t\r\n",
        "\r\n",
        RowSegment
        ]),
    gen_tcp:send(Sock, Resp).
