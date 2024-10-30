-module(vstreamer_router).

-export([router/3]).

router(<<"GET">>, <<"/">>, _) ->
    {302, <<"Location: /page\r\n">>};

router(<<"GET">>, <<"/page">>, _) ->
    vstreamer_handler:page_handler(<<"index">>);

router(<<"GET">>, <<"/page/", PageName/binary>>, _) ->
    vstreamer_handler:page_handler(PageName);

router(<<"GET">>, <<"/stream/", VideoPath/binary>>, _) ->
    vstreamer_handler:stream_handler(VideoPath);

router(<<"GET">>, <<"/videos">>, _) ->
    vstreamer_handler:video_handler();

router(<<"POST">>, <<"/upload">>, Body) ->
    vstreamer_handler:upload_handler(Body);

router(_, _, _) ->
    Header = vstreamer_http:serialize_header([
        {<<"Content-Type">>, <<"text/plain">>}
    ]),
    {404, Header, <<"Not Found">>}.