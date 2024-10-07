-module(vstreamer_handler).

-export([page_handler/1, stream_handler/1, upload_handler/1]).

-import(vstreamer_http, [parse_header/2, serialize_header/1]).
-import(vstreamer_videos, [load_video/1, is_exist_video/1, download_video/2, get_video_list/0]).
-import(vstreamer_pages, [read_page/1, embed_data/3]).

page_handler(<<"list">>) ->
    {ok, File} = read_page(<<"list">>),
    Header = serialize_header([
        {<<"Content-Type">>, <<"text/html">>}
    ]),
    VideoLinks = lists:concat([
        "<li><a href=\"/page/video/" ++ Video ++ "\">" ++ Video ++ "</a></li>" || Video <- get_video_list()
    ]),
    {200, Header, embed_data(File, "%%VIDEO_LIST%%", VideoLinks)};

page_handler(<<"video/", VideoName/binary>>) ->
    Header = serialize_header([
        {<<"Content-Type">>, <<"text/html">>}
    ]),
    case is_exist_video(VideoName) of
        true ->
            {ok, File} = read_page(<<"video">>),
            {200, Header, embed_data(File, "%%VIDEO_NAME%%", binary_to_list(VideoName))};
        false ->
            {ok, File} = read_page(<<"404">>),
            {404, Header, File}
    end;

page_handler(Page) ->
    Header = serialize_header([
        {<<"Content-Type">>, <<"text/html">>}
    ]),
    case read_page(Page) of
        {ok, File} ->
            {200, Header, File};
        {error, File} ->
            {404, Header, File}
    end.

stream_handler(VideoPath) ->
    case load_video(VideoPath) of
        {manifest, File} ->
            Header = serialize_header([
                {<<"Content-Type">>, <<"video/mp4">>},
                {<<"Access-Control-Allow-Origin">>, <<"*">>}
            ]),
            {200, Header, File};
        {segment, File} ->
            Header = serialize_header([
                {<<"Content-Type">>, <<"video/mp4">>},
                {<<"Access-Control-Allow-Origin">>, <<"*">>}
            ]),
            {200, Header, File};
        {error, _} ->
            Header = serialize_header([
                {<<"Content-Type">>, <<"text/plain">>}
            ]),
            {404, Header, <<"Not found!">>}
    end.

upload_handler(Body) ->
    case extract_video(Body) of
        {ok, VideoName, ExtractVideo} ->
            spawn(fun() -> download_video(VideoName, ExtractVideo) end),
            Header = serialize_header([
                {<<"Content-Type">>, <<"text/plain">>}
            ]),
            {201, Header, <<"Upload page">>};
        error ->
            Header = serialize_header([
                {<<"Content-Type">>, <<"text/plain">>}
            ]),
            {500, Header, <<"Failed to update video">>}
    end.

extract_video(Body) ->
    [Header, Video] = string:split(Body, "\r\n\r\n"),
    [Delim | _ ] = string:split(Header, "\r\n", all),
    case maps:find("filename", 
        parse_header(string:split(
        replace_multiple_words(binary_to_list(Header), [{binary_to_list(Delim), ""}, {"; ", "\r\n"}, {"=", ": "}, {"\"", ""}]),
        "\r\n", all), #{})
    ) of
        {ok, VideoName} ->
            [ExtractVideo, _] = string:split(Video, <<"\r\n", Delim/binary>>),
            {ok, VideoName, ExtractVideo};
        error -> error
    end.

replace_multiple_words(Text, Replacements) ->
    case Replacements of
        [] -> Text;
        [{Old, New} | Rest] -> replace_multiple_words(string:replace(Text, Old, New, all), Rest)
    end.
