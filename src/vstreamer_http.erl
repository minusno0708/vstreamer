-module(vstreamer_http).

-export([parse_http/1, serialize_http/2, serialize_http/3, conn_body/2]).

status_msg(StatusCode) ->
    case StatusCode of
        200 -> <<"200 OK">>;
        201 -> <<"201 Created">>;
        204 -> <<"204 No Content">>;
        302 -> <<"302 Found">>;
        400 -> <<"400 Bad Request">>;
        404 -> <<"404 Not Found">>;
        _ -> <<"500 Internal Server Error">>
    end.

parse_http(Req) ->
    [HeaderSection | BodySection] = string:split(Req, "\r\n\r\n", all),
    [StatusLine | HeaderLine] = string:split(HeaderSection, "\r\n", all),

    Status = string:split(StatusLine, " ", all),
    Header = parse_header(HeaderLine, #{}),
    Body = conn_body(BodySection, <<>>),

    {Status, Header, Body}.

parse_header(HeaderList, HeaderMap) ->
    case HeaderList of
        [] -> HeaderMap;
        [Header | Rest] ->
            case string:split(Header, ": ", all) of
                [Key, Value] -> 
                    parse_header(Rest, HeaderMap#{Key => Value});
                _ -> 
                    parse_header(Rest, HeaderMap)
            end
    end.

conn_body(BodySection, Body) ->
    case BodySection of
        [] -> Body;
        [BodyHead] -> <<Body/binary, BodyHead/binary>>;
        [BodyHead | BodyTail] ->
            conn_body(BodyTail, <<Body/binary, BodyHead/binary, "\r\n\r\n">>)
    end.  

serialize_http(StatusCode, Header) ->
    <<
        "HTTP/1.1 ", (status_msg(StatusCode))/binary, " \r\n",
        Header/binary,
        "\r\n"
    >>.

serialize_http(StatusCode, Header, Body) ->
    <<
        "HTTP/1.1 ", (status_msg(StatusCode))/binary, " \r\n",
        "Content-Length: ", (integer_to_binary(byte_size(Body)))/binary, "\r\n",
        Header/binary,
        "\r\n",
        Body/binary
    >>.

