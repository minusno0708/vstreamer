-module(vstreamer_database).

-export([
    connect_db/0,
    close_db/1,
    query/2, query/3]).

connect_db() ->
    mysql:start_link([
        {host, "localhost"},
        {user, "root"},
        {password, "root"},
        {database, "contents_db"}]).

close_db(Pid) ->
    mysql:stop(Pid).

query(Pid, Query) ->
    mysql:query(Pid, Query).

query(Pid, Query, Params) ->
    mysql:query(Pid, Query, Params).