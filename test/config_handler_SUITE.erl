-module(config_handler_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

groups() ->
	[{config_handler_group, [], [testidsiin}].

all() ->
	[{group, config_handler_group)}]

init_per_group(_GroupName, _Config) ->
	application:set_env(elector, strategy_module, runtime_strategy),
	ok.

end_per_group(_GroupName, _Config) ->
	ok.