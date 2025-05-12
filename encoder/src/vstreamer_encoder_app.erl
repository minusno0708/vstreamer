%%%-------------------------------------------------------------------
%% @doc vstreamer_encoder public API
%% @end
%%%-------------------------------------------------------------------

-module(vstreamer_encoder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    vstreamer_encoder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
