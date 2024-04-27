-module(files).
-export([read_page/1, load_video/1, encode_video/1]).

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

encode_video(FileName) ->
    os:cmd("mkdir -p ../videos/" ++ FileName),

    Path = "../videos/" ++ FileName ++ ".mp4",
    Output = "../videos/" ++ FileName ++ "/manifest.mpd",

    BitConfig = [
        {row, "1M -s 720x480"},
        {medium, "2M -s 1280x720"},
        {high, "5M -s 1920x1080"}   
    ],

    Command = lists:concat([
        "ffmpeg -i ",
        Path,
        " -c:v libx264 -b:v ",
        proplists:get_value(medium, BitConfig),
        " -keyint_min 150 -g 150 -sc_threshold 0 -profile:v high -preset slow -an -f dash ",
        Output
    ]),
    os:cmd(Command).
