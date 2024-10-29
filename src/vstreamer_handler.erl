-module(vstreamer_handler).

-export([page_handler/1, stream_handler/1, upload_handler/1]).

page_handler(<<"list">>) ->
    {ok, File} = vstreamer_pages:read_page(<<"list">>),
    Header = html_content_header(),
    VideoLinks = lists:concat([
        "<li><a href=\"/page/video/" ++ Video ++ "\">" ++
        Video ++
        "</a></li>"
        || Video <- vstreamer_videos:get_video_list()]
    ),
    {200, Header, vstreamer_pages:embed_data(File, "%%VIDEO_LIST%%", VideoLinks)};

page_handler(<<"video/", VideoName/binary>>) ->
    Header = html_content_header(),
    case vstreamer_videos:is_exist_video(VideoName) of
        true ->
            {ok, File} = vstreamer_pages:read_page(<<"video">>),
            {200, Header,
                vstreamer_pages:embed_data(File, "%%VIDEO_NAME%%", binary_to_list(VideoName))};
        false ->
            {ok, File} = vstreamer_pages:read_page(<<"404">>),
            {404, Header, File}
    end;

page_handler(Page) ->
    Header = html_content_header(),
    case vstreamer_pages:read_page(Page) of
        {ok, File} ->
            {200, Header, File};
        {error, File} ->
            {404, Header, File}
    end.

stream_handler(VideoPath) ->
    case vstreamer_videos:load_video(VideoPath) of
        {manifest, File} ->
            Header = mp4_content_header(),
            {200, Header, File};
        {segment, File} ->
            Header = mp4_content_header(),
            {200, Header, File};
        {error, _} ->
            Header = plain_content_header(),
            {404, Header, <<"Not found!">>}
    end.

upload_handler(Body) ->
    case extract_video(Body) of
        {ok, VideoName, ExtractVideo} ->
            spawn(fun() -> vstreamer_enc_manager:encode_manager(VideoName, ExtractVideo) end),
            Header = plain_content_header(),
            {201, Header, <<"Upload page">>};
        error ->
            Header = plain_content_header(),
            {500, Header, <<"Failed to update video">>}
    end.

plain_content_header() -> vstreamer_http:serialize_header([
    {<<"Content-Type">>, <<"text/plain">>}
]).

html_content_header() -> vstreamer_http:serialize_header([
    {<<"Content-Type">>, <<"text/html">>}
]).

mp4_content_header() -> vstreamer_http:serialize_header([
    {<<"Content-Type">>, <<"video/mp4">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}
]).

extract_video(Body) ->
    [Header, Video] = string:split(Body, "\r\n\r\n"),
    [Delim | _ ] = string:split(Header, "\r\n", all),
    case maps:find("filename",
        vstreamer_http:parse_header(string:split(
        replace_multiple_words(
            binary_to_list(Header),
            [{binary_to_list(Delim), ""}, {"; ", "\r\n"}, {"=", ": "}, {"\"", ""}]
        ),
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
