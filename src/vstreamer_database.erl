-module(vstreamer_database).

-export([
    connectDB/0,
    closeDB/1,
    query/2, query/3]).

connectDB() ->
    mysql:start_link([
        {host, "localhost"},
        {user, "root"},
        {password, "root"},
        {database, "contents_db"}]).

closeDB(Pid) ->
    mysql:stop(Pid).

query(Pid, Query) ->
    mysql:query(Pid, Query).

query(Pid, Query, Params) ->
    mysql:query(Pid, Query, Params).