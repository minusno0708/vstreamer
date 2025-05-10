%%%-------------------------------------------------------------------
%% @doc encoder public API
%% @end
%%%-------------------------------------------------------------------

-module(encoder_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    encoder_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
