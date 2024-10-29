-module(vstreamer_videos).

-export([is_exist_video/1, load_video/1, get_video_list/0]).

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

get_video_list() ->
    [hd(lists:reverse(string:replace(Path, "videos/", ""))) ||
        Path <- filelib:wildcard("videos/*"),
        filelib:is_dir(Path)
    ].