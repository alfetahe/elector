-module(runtime_high_strategy).
-behaviour(strategy_behaviour).
-export([elect/0]).

elect() ->
	runtime_strategy_base:elect(high).
