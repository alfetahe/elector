-module(config_handler_SUITE).
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([election_delay_test/1, strategy_module_test/1, sync_start_test/1,
pre_election_hooks_test/1, post_election_hooks_test/1]).

groups() ->
	[{config_handler_group, [], [election_delay_test,
	strategy_module_test, sync_start_test, pre_election_hooks_test,
	post_election_hooks_test]}].

all() ->
	[{group, config_handler_group}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	application:set_env(elector, election_delay, 3000),
	application:set_env(elector, strategy_module, runtime_strategy),
	application:set_env(elector, sync_start, false),
	application:set_env(elector, pre_election_hooks, []),
	application:set_env(elector, post_election_hooks, []).

election_delay_test(_Config) ->
    application:set_env(elector, election_delay, 5000),
	config_handler:election_delay() == 5000.

strategy_module_test(_Config) ->
    application:set_env(elector, strategy_module, test_module),
	config_handler:strategy_module() == test_module.

sync_start_test(_Config) ->
    application:set_env(elector, sync_start, true),
	config_handler:sync_start() == true.

pre_election_hooks_test(_Config) ->
	application:set_env(elector, pre_election_hooks, test_hooks()),
	config_handler:pre_election_hooks() == test_hooks().

post_election_hooks_test(_Config) ->
	application:set_env(elector, post_election_hooks, test_hooks()),
	config_handler:post_election_hooks() == test_hooks().

test_hooks() ->
	[{test, test, []}].
	