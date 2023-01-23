-module(runtime_strategy_base).
-export([elect/1, host_node_runtime/0]).

elect(Type) ->
	Nodes = rpc_client:connected_nodes(),
	ExternalNodeRuntimes = iterate_runtimes(Nodes, #{}),
	Runtimes = maps:put(node(), host_node_runtime(), ExternalNodeRuntimes),
	choose_leader(Runtimes, Type).

host_node_runtime() ->
	{Runtime, _TimeSinceLastCall} = erlang:statistics(runtime),
	Runtime.

choose_leader(Runtimes, Type) ->
	Fn = fun(Node, Runtime, {IterationNode, IterationRuntime} = CurrentlySelected) -> 
		if
			(IterationNode == nil) orelse
			(Type =:= high andalso Runtime >= IterationRuntime) orelse 
			(Type =:= low andalso Runtime =< IterationRuntime) ->
				{Node, Runtime};
			true ->
				CurrentlySelected
		end
	end,

	{Node, _Runtime} = maps:fold(Fn, {nil, 0}, Runtimes),
	Node.

iterate_runtimes([Node], Runtimes) ->
	{_, Runtime} = rpc_client:call(Node, erlang, statistics, [runtime]),
	maps:put(Node, Runtime, Runtimes);

iterate_runtimes([Node | Nbody], Runtimes) ->
	{_, Runtime} = rpc_client:call(Node, erlang, statistics, [runtime]),
	Runtimes = maps:put(Node, Runtime, Runtimes),
	iterate_runtimes(Nbody, Runtimes);

iterate_runtimes([], Runtimes) ->
	Runtimes.