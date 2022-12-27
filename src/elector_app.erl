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
    Strategy_module = config_handler:strategy_module(),
    
    Attributes = erlang:apply(Strategy_module, module_info, [attributes]),
    Behaviours = proplists:get_value(behaviour, Attributes),
    Is_strategy = lists:member(strategy_behaviour, Behaviours),

    if 
        Is_strategy /= true ->
            throw({strategy_implementation_error, "Strategy module must implement the base strategy behaviour"});
        true ->
            nil
    end,

    ok.
