-module(stream).
-export([load_video/1]).

load_video(FileName) ->
    Path = "../videos/" ++ binary_to_list(FileName) ++ ".mp4", 
    case file:read_file(Path) of
        {ok, File} ->
            PlainFile = binary_to_list(File),
            {ok, PlainFile};
        {error, Reason} ->
            {error, Reason}
    end.