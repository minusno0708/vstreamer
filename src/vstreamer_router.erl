-module(vstreamer_router).

-export([router/3]).

-import(vstreamer_http, [serialize_header/2]).
-import(vstreamer_handler, [page_handler/1, stream_handler/1, upload_handler/1]).

router(<<"GET">>, <<"/">>, _) ->
    {302, <<"Location: /page\r\n">>};

router(<<"GET">>, <<"/page">>, _) ->
    page_handler(<<"index">>);

router(<<"GET">>, <<"/page/", PageName/binary>>, _) ->
    page_handler(PageName);

router(<<"GET">>, <<"/stream/", VideoPath/binary>>, _) ->
    stream_handler(VideoPath);

router(<<"POST">>, <<"/upload">>, Body) ->
    upload_handler(Body);

router(_, _, _) ->
    Header = serialize_header([
        {<<"Content-Type">>, <<"text/plain">>}
    ], <<>>),
    {404, Header, <<"Not found!">>}.