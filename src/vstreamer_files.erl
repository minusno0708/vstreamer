-module(files).
-export([read_page/1, is_exist_video/1, load_video/1, encode_video/1, download_video/2, get_video_list/0]).

read_page(FileName) ->
    case file:read_file("pages/" ++ binary_to_list(FileName) ++ ".html") of
        {ok, File} -> 
            {ok, File};
        {error, enoent} -> 
            {ok, File} = file:read_file("pages/404.html"),
            {error, File}
    end.

is_exist_video(VideoName) ->
    case filelib:is_dir("videos/" ++ binary_to_list(VideoName)) of
        true -> true;
        false -> false
    end.

load_video(VideoPath) ->
    ListVideoPath = binary_to_list(VideoPath),
    case file:read_file("videos/" ++ ListVideoPath) of
        {ok, File} ->
            case string:split(ListVideoPath, ".", all) of
                [_, "mpd"] -> {manifest, File};
                [_, "m4s"] -> {segment, File};
                _ -> {error, "Invalid file type"}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

encode_video(FileName) ->
    os:cmd("mkdir -p videos/" ++ FileName),

    Path = "videos/" ++ FileName ++ ".mp4",
    Output = "videos/" ++ FileName ++ "/manifest.mpd",

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
        " -keyint_min 150 -g 150 -profile:v high -preset medium -c:a aac -ac 2 -b:a 128k -f dash ",
        Output
    ]),
    os:cmd(Command).

download_video(Name, Body) ->
    {ok, File} = file:open("videos/" ++ Name, [write]),
    file:write(File, Body),
    file:close(File),

    encode_video(string:replace(Name, ".mp4", "")),
    os:cmd("rm videos/" ++ Name).

get_video_list() ->
    FileList = filelib:wildcard("videos/*"),
    VideoList = get_video_list([], FileList),
    VideoList.

get_video_list(VideoList, FileList) ->
    case FileList of
        [] -> VideoList;
        [H | T] ->
            FileName = hd(lists:reverse(string:replace(H, "videos/", ""))),
            case string:str(FileName, ".") of
                0 -> get_video_list([VideoList | embed_video_link(FileName)], T);
                _ -> get_video_list(VideoList, T)
            end
    end.

embed_video_link(VideoName) ->
    lists:concat([
        "<li>",
        "<a href=\"/video/",
        VideoName,
        "\">",
        VideoName,
        "</a>",
        "</li>"
    ]).
