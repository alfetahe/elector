-module(runtime_strategy).

-behaviour(strategy_behaviour).

-export([elect/0]).

elect() ->
	Nodes = rpc_client:connected_nodes(),
	Runtimes = iterate_runtimes(Nodes, #{
		node() => host_node_runtime()
	}),
	choose_leader(Runtimes).

iterate_runtimes([], Runtimes) ->
	Runtimes;
iterate_runtimes[Node | Nbody], Runtimes) ->
	{_, Runtime} = rpc_client:call(Node, erlang, statistics, [runtime]),
	Runtimes = maps:put(Node, Runtime, Runtimes),
	iterate(Nbody, Runtimes).

host_node_runtime() ->
	{_, Runtime} = erlang:statistics(runtime),
	Runtime

choose_leader(Runtimes) ->
	{Node, Runtime} = maps:fold(fun(Node, Runtime, {Highest_node, Highest_runtime} = Acc) -> 
		if
		Runtime >= Highest_runtime ->
			Runtime;
		true ->
      Acc
	end, {nil, 0}, Runtimes),
	Node.

	

	
	
