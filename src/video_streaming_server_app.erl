%%%-------------------------------------------------------------------
%% @doc video_streaming_server public API
%% @end
%%%-------------------------------------------------------------------

-module(video_streaming_server_app).

-behaviour(application).

-export([start/2, stop/1]).

-import(server, [start/1]).

start(_StartType, _StartArgs) ->
    server:start(8080),
    video_streaming_server_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
