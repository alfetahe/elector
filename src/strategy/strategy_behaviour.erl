-module(strategy_behaviour).
-callback elect() -> Leader :: leader().
-export([elect/0]).
-type leader() :: node().

elect() ->
	Strategy_module = application:get_env(elector, strategy_module, runtime_high_strategy),
	erlang:apply(Strategy_module, elect, []).