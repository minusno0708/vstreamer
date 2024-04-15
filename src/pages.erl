-module(pages).
-export([read_page/1]).

read_page(PageName) ->
    Path = "../pages/" ++ PageName ++ ".html",
    case file:read_file(Path) of
        {ok, File} -> 
            PlainFile = binary_to_list(File),
            {ok, PlainFile};
        {error, enoent} -> 
            {error, "Page Not Found"}
    end.
