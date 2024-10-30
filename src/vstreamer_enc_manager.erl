-module(vstreamer_enc_manager).

-export([encode_manager/2]).

encode_manager(Name, Body) ->
    VideoID = vstreamer_videos:register_video(string:replace(Name, ".mp4", "")),
    save_raw_video(VideoID, Body),
    encode(VideoID),
    remove_raw_video(VideoID).

raw_video_path(VideoID) ->
    <<"videos/", VideoID/binary, ".mp4">>.

save_raw_video(VideoID, Body) ->
    {ok, File} = file:open(raw_video_path(VideoID), [write]),
    file:write(File, Body),
    file:close(File).

remove_raw_video(VideoID) ->
    os:cmd(binary_to_list(<<"rm ", (raw_video_path(VideoID))/binary>>)).

encode(VideoID) ->
    os:cmd(binary_to_list(<<"mkdir -p videos/", VideoID/binary>>)),

    Path = raw_video_path(VideoID),
    Output = <<"videos/", VideoID/binary, "/manifest.mpd">>,

    BitConfig = [
        {row, <<"1M -s 720x480">>},
        {medium, <<"2M -s 1280x720">>},
        {high, <<"5M -s 1920x1080">>}
    ],

    os:cmd(binary_to_list(<<
        "ffmpeg -i ",
        Path/binary,
        " -c:v libx264 -b:v ",
        (proplists:get_value(medium, BitConfig))/binary,
        " -keyint_min 150 -g 150 -profile:v high -preset medium -c:a aac -ac 2 -b:a 128k -f dash ",
        Output/binary
    >>)).
