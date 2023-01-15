-module(runtime_low_strategy_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_elect/1]).

groups() ->
	[{runtime_low_strategy_group, [], [test_elect]}].

all() ->
	[{group, runtime_low_strategy_group}].

init_per_group(_GroupName, Config) ->
	{_, CurrRuntime} = erlang:statistics(runtime),
	[{curr_runtime, CurrRuntime} | Config].

end_per_group(_GroupName, _Config) ->
	ok.

test_elect(_Config) ->
	SelectedNode = runtime_low_strategy:elect(),
	SelectedNode == node().
