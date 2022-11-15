%%%-------------------------------------------------------------------
%% @doc elector public API
%% @end
%%%-------------------------------------------------------------------

-module(elector_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    elector_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
