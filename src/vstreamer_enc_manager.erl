-module(vstreamer_enc_manager).

-export([encode_manager/2]).

encode_manager(Name, Body) ->
    save_raw_video(Name, Body),
    encode(string:replace(Name, ".mp4", "")),
    remove_raw_video("videos/" ++ Name).

save_raw_video(Name, Body) ->
    {ok, File} = file:open("videos/" ++ Name, [write]),
    file:write(File, Body),
    file:close(File).

remove_raw_video(Path) ->
    os:cmd("rm " ++ Path).

encode(FileName) ->
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