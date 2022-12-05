-module(runtime_strategy_SUITE).

host_node_runtime_test(_Config) ->
	CurrRuntime = 
	Resp = host_node_runtime(),
	assert?(is_integer(Resp) and CurrRuntime < Resp).