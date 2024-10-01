-module(vstreamer_pages).

-export([read_page/1]).

read_page(FileName) ->
    case file:read_file("pages/" ++ binary_to_list(FileName) ++ ".html") of
        {ok, File} -> 
            {ok, File};
        {error, enoent} -> 
            {ok, File} = file:read_file("pages/404.html"),
            {error, File}
    end.