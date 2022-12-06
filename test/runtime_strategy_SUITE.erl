-module(runtime_strategy_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

groups() ->
	[{runtime_strategy_group, [], [elect_test, host_node_runtime_test}].

all() ->
	[{group, runtime_strategy_group)}]

suite() ->
	{_, CurrRuntime} = erlang:statistics(runtime),
	[{curr_runtime, CurrRuntime}].

init_per_group(_GroupName, _Config) ->
	ok.

end_per_group(_GroupName, _Config) ->
	ok.

init_per_suite(_Config) ->
	ok.

end_per_suite(_Config) ->
	ok.

host_node_runtime_test(_Config) ->
	Resp = runtime_startegy:host_node_runtime(),
	assert?(is_integer(Resp) and CurrRuntime =< Resp).

elect_test(_Config) ->
	application:set_env(elector, strategy_module, runtime_strategy),
	SelectedNode = runtime_strategy:elect(),
	assert?(SelectedNode =:= node()).
