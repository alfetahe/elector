-module(runtime_strategy_base).
-export([elect/1, host_node_runtime/0]).

elect(Type) ->
	Nodes = rpc_client:connected_nodes(),
	Runtimes = iterate_runtimes(Nodes, #{
		node() => host_node_runtime()
	}),
	choose_leader(Runtimes, Type).

host_node_runtime() ->
	{_, Runtime} = erlang:statistics(runtime),
	Runtime.

choose_leader(Runtimes, Type) ->
	Fn = fun(Node, Runtime, {_Iteration_node, Iteration_runtime} = Acc) -> 
		if
			(Type =:= high andalso Runtime >= Iteration_runtime) orelse 
			(Type =:= low andalso Runtime =< Iteration_runtime) ->
				{Node, Runtime};
			true ->
      			Acc
		end
	end,
	{Node, _Runtime} = maps:fold(Fn, {nil, 0}, Runtimes),
	Node.

iterate_runtimes([], Runtimes) ->
	Runtimes;

iterate_runtimes([Node | Nbody], Runtimes) ->
	{_, Runtime} = rpc_client:call(Node, erlang, statistics, [runtime]),
	Runtimes = maps:put(Node, Runtime, Runtimes),
	iterate_runtimes(Nbody, Runtimes).