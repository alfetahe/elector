-module(runtime_strategy).

-behaviour(strategy_behaviour).

-export([elect/0]).

elect() ->
	Nodes = rpc_client:connected_nodes(),
	Runtimes = iterate(Nodes, #{}),
	ok.

iterate([], Runtimes) ->
	Runtimes;
iterate[Node | Nbody], Runtimes) ->
	Runtime = rpc_client:call(Node, test, test, []),
	Runtimes = maps:put(Node, Runtime, Runtimes),
	iterate(Nbody, Runtimes).


	
