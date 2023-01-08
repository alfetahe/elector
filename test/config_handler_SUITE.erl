-module(config_handler_SUITE).
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_election_delay/1, test_strategy_module/1, test_sync_start/1,
test_pre_election_hooks/1, test_post_election_hooks/1, test_hook_trigger/1]).

groups() ->
	[{config_handler_group, [], [test_election_delay,
	test_strategy_module, test_sync_start, test_pre_election_hooks,
	test_post_election_hooks]}].

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

test_election_delay(_Config) ->
    application:set_env(elector, election_delay, 5000),
	config_handler:election_delay() == 5000.

test_strategy_module(_Config) ->
    application:set_env(elector, strategy_module, test_module),
	config_handler:strategy_module() == test_module.

test_sync_start(_Config) ->
    application:set_env(elector, sync_start, true),
	config_handler:sync_start() == true.

test_pre_election_hooks(_Config) ->
	application:set_env(elector, pre_election_hooks, test_hook(pre)),
	elector:elect_sync(),
	Cond1 = config_handler:pre_election_hooks() == test_hook(pre),
	TriggerType = trigger_type(pre),
	Cond2 = receive
		TriggerType -> 
			true
	after
		2000 -> false
	end,
	Cond1 == true andalso Cond2 == true.

test_post_election_hooks(_Config) ->
	application:set_env(elector, post_election_hooks, test_hook(post)),
	elector:elect_sync(),
	Cond1 = config_handler:post_election_hooks() == test_hook(post),
	TriggerType = trigger_type(post),
	Cond2 = receive
		TriggerType -> 
			true
	after
		2000 -> false
	end,
	Cond1 == true andalso Cond2 == true.

test_hook_trigger(Type) ->
	erlang:send(self(), trigger_type(Type)).

test_hook(Type) ->
	[{?MODULE, trigger_type(Type), [self()]}].

trigger_type(Type) ->
	case Type of
		pre -> test_pre_hook_trigger;
		post -> test_post_hook_trigger
	end.
	