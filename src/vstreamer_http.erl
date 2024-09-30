-module(vstreamer_http).

-export([serialize_http/2, serialize_http/3]).

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

