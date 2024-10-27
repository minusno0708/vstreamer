%%%-------------------------------------------------------------------
%% @doc vstreamer public API
%% @end
%%%-------------------------------------------------------------------

-module(vstreamer_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    logger:set_primary_config(level, info),
    vstreamer_server:run(8080),
    vstreamer_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
