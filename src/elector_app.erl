%%%-------------------------------------------------------------------
%% @doc elector public API
%% @end
%%%-------------------------------------------------------------------

-module(elector_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    validate(),
    
    elector_sup:start_link().

stop(_State) ->
    ok.

%%----------------------------------------------------------------------
%% internal functions
%%----------------------------------------------------------------------

validate() ->
    Strategy_module = application:get_env(elector, 
                                          strategy_module,
                                          runtime_strategy),
    
    %[{exports, items}] = Strategy_module:module_info(),    
    ok.
