-module(files).
-export([read_page/1, encode_video/1]).

read_page(FileName) ->
    Path = "../pages/" ++ binary_to_list(FileName) ++ ".html",
    case file:read_file(Path) of
        {ok, File} -> 
            PlainFile = binary_to_list(File),
            {ok, PlainFile};
        {error, enoent} -> 
            {ok, File} = file:read_file("../pages/404.html"),
            PlainFile = binary_to_list(File),
            {error, PlainFile}
    end.

encode_video(FileName) ->
    Path = "../videos/" ++ FileName ++ ".mp4",
    Output = "../videos/" ++ FileName ++ ".m3u8",

    Command = lists:concat([
        "ffmpeg -i ",
        Path,
        " -g 60 -hls_time 2 -hls_list_size 0 -hls_segment_size 500000 ",
        Output
    ]),
    os:cmd(Command).
