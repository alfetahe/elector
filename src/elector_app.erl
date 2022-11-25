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
    
    attributes = Strategy_module:module_info(attributes),
		behaviours = proplists:get_value(behaviour),

		if 
			lists:member(strategy_behaviour, behaviours) /= true ->
				throw({strategy_implementation_error, "Strategy module must implement the base strategy behaviour"})
                
        end,
    ok.
