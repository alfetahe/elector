-module(runtime_strategy_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_host_node_runtime/1, test_elect/1]).

groups() ->
	[{runtime_strategy_group, [], [test_host_node_runtime, test_elect]}].

all() ->
	[{group, runtime_strategy_group}].

init_per_group(_GroupName, Config) ->
	{_, CurrRuntime} = erlang:statistics(runtime),
	[{curr_runtime, CurrRuntime} | Config].

end_per_group(_GroupName, _Config) ->
	ok.

test_host_node_runtime(Config) ->
	Resp = runtime_strategy:host_node_runtime(),
	Runtime = ?config(curr_runtime, Config),
	is_integer(Resp) andalso Runtime =< Resp.

test_elect(_Config) ->
	SelectedNode = runtime_strategy:elect(),
	SelectedNode == node().
