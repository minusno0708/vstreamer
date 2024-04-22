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
    Resp = 
        <<
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: ", (length(binary_to_list(Playlist))), "\r\n",
        "Content-Type: application/vnd.apple.mpegurl\r\n",
        "\r\n",
        Playlist/binary
        >>,
    gen_tcp:send(Sock, Resp).

send_segment(Sock, Segment) ->
    Resp = 
        <<
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: ", (length(binary_to_list(Segment))), "\r\n",
        "Content-Type: video/mp2t\r\n",
        "\r\n",
        Segment/binary
        >>,
    gen_tcp:send(Sock, Resp).
