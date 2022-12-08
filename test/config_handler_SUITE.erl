-module(config_handler_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

groups() ->
	[{config_handler_group, [], [testidsiin]}].

all() ->
	[{group, config_handler_group}].

end_per_group(_GroupName, _Config) ->
	application:set_env(elector, election_delay, 3000),
	application:set_env(elector, strategy_module, runtime_strategy),
	application:set_env(elector, sync_start, false),
	application:set_env(elector, pre_election_hooks, []),
	application:set_env(elector, post_election_hooks, []).

election_delay_test() ->
    application:set_env(elector, election_delay, 5000),
	?assert(config_handler:election_delay() =:= 5000).

strategy_module() ->
    application:set_env(elector, strategy_module, test_module),
	?assert(config_handler:strategy_module() =:= test_module).

sync_start() ->
    application:set_env(elector, sync_start, true),
	?assert(config_handler:sync_start() =:= true).

pre_election_hooks() ->
	application:set_env(elector, pre_election_hooks, test_hooks()),
	?assert(config_handler:pre_election_hooks() =:= test_hooks()).

post_election_hooks() ->
	application:set_env(elector, post_election_hooks, test_hooks()),
	?assert(config_handler:post_election_hooks() =:= test_hooks()).

test_hooks() ->
	[{test, test, []}].
	