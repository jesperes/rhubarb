%%%-------------------------------------------------------------------
%% @doc rhubarb public API
%% @end
%%%-------------------------------------------------------------------

-module(rhubarb_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    rhubarb_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
