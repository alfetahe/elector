-module(runtime_strategy_SUITE).

host_node_runtime_test(_Config) ->
	{_, CurrRuntime} = eroang:statistics(runtime),
	Resp = runtime_startegy:host_node_runtime(),
	assert?(is_integer(Resp) and CurrRuntime =< Resp).

elect_test(_Config) ->
	SelectedNode = runtime_strategy:elect(),
	assert?(SelectedNode =:= node()).
