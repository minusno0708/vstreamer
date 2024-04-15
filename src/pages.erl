-module(pages).
-export([read_page/1]).

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
