-module(stream).
-export([load_video/1, stream/2]).

load_video(FileName) ->
    Path = "../videos/" ++ binary_to_list(FileName) ++ ".mp4", 
    case file:read_file(Path) of
        {ok, File} ->
            PlainFile = binary_to_list(File),
            {ok, PlainFile};
        {error, Reason} ->
            {error, Reason}
    end.

stream(Sock, Video) ->
    Resp = 
        lists:concat([
        "HTTP/1.1 200 OK \r\n",
        "Content-Length: " ++ integer_to_list(length(Video)) ++ "\r\n",
        "Content-Type: video/mp4\r\n",
        "\r\n",
        Video
        ]),
    gen_tcp:send(Sock, Resp).