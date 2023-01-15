-module(runtime_strategy_base_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_host_node_runtime/1]).

groups() ->
	[{runtime_strategy_base_group, [], [test_host_node_runtime]}].

all() ->
	[{group, runtime_strategy_base_group}].

init_per_group(_GroupName, Config) ->
	{_, CurrRuntime} = erlang:statistics(runtime),
	[{curr_runtime, CurrRuntime} | Config].

end_per_group(_GroupName, _Config) ->
	ok.

test_host_node_runtime(Config) ->
	Resp = runtime_strategy_base:host_node_runtime(),
	Runtime = ?config(curr_runtime, Config),
	is_integer(Resp) andalso Runtime =< Resp.