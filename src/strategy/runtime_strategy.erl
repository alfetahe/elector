-module(runtime_strategy).

-behaviour(strategy_behaviour).

-export([elect/0]).

host_node_runtime() ->
	{_, Runtime} = erlang:statistics(runtime),
	Runtime.

choose_leader(Runtimes) ->
	Fn = fun(Node, Runtime, {_Highest_node, Highest_runtime} = Acc) -> 
		if
			Runtime >= Highest_runtime ->
				{Node, Runtime};
			true ->
      			Acc	
		end
	end,
	{Node, _Runtime} = maps:fold(Fn, {nil, 0}, Runtimes),
	Node.

elect() ->
	Nodes = rpc_client:connected_nodes(),
	Runtimes = iterate_runtimes(Nodes, #{
		node() => host_node_runtime()
	}),
	choose_leader(Runtimes).

iterate_runtimes([], Runtimes) ->
	Runtimes;
iterate_runtimes([Node | Nbody], Runtimes) ->
	{_, Runtime} = rpc_client:call(Node, erlang, statistics, [runtime]),
	Runtimes = maps:put(Node, Runtime, Runtimes),
	iterate_runtimes(Nbody, Runtimes).


	

	
	
