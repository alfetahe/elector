-module(strategy_behaviour).

-type leader() :: node().

-callback elect() -> Leader :: leader().

-export([elect/0]).

elect() ->
	Strategy_module = application:get_env(elector, strategy_module, runtime_strategy),
	erlang:apply(Strategy_module, elect, []).