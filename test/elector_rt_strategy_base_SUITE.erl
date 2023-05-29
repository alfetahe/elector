-module(elector_time_strategy_base_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([groups/0, all/0, init_per_group/2, end_per_group/2]).

groups() ->
    [{elector_time_strategy_base_group, [], []}].

all() ->
    [{group, elector_time_strategy_base_group}].

init_per_group(_GroupName, Config) ->
    {_, CurrRuntime} = erlang:statistics(runtime),
    [{curr_runtime, CurrRuntime} | Config].

end_per_group(_GroupName, _Config) ->
    ok.
