-module(runtime_low_strategy).
-behaviour(strategy_behaviour).
-export([elect/0]).

elect() ->
	runtime_strategy_base:elect(low).


	

	
	
