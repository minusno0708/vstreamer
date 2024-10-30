-module(vstreamer_videos).

-export([is_exist_video/1, register_video/1, load_video/1, get_video_list/0]).

is_exist_video(VideoName) ->
    case filelib:is_dir("videos/" ++ binary_to_list(VideoName)) of
        true -> true;
        false -> false
    end.

generate_uuid() ->
    list_to_binary(hd(string:tokens(os:cmd("uuidgen"), "\n"))).

register_video(VideoName) ->
    VideoID = generate_uuid(),
    {ok, Pid} = vstreamer_database:connectDB(),
    mysql:query(
        Pid,
        "INSERT INTO videos (id, title) VALUES (?, ?)",
        [VideoID, VideoName]
    ),
    VideoID.

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
    {ok, Pid} = vstreamer_database:connectDB(),
    {ok, Col, Rows} = mysql:query(Pid, "SELECT id, title FROM videos"),
    lists:map(
        fun(Row) -> maps:from_list(lists:zip(Col, Row)) end,
        Rows
    ).
        