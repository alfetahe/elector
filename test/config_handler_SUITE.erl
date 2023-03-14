-module(config_handler_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_election_delay/1, test_strategy_module/1, test_sync_start/1,
         test_pre_election_hooks/1, test_post_election_hooks/1, test_startup_hooks_enabled/1]).

groups() ->
    [{config_handler_group,
      [],
      [test_election_delay,
       test_strategy_module,
       test_sync_start,
       test_pre_election_hooks,
       test_post_election_hooks,
       test_startup_hooks_enabled]}].

all() ->
    [{group, config_handler_group}].

init_per_group(_GroupName, Config) ->
    application:ensure_started(elector),
    Config.

end_per_group(_GroupName, _Config) ->
    application:set_env(elector, election_delay, 1000),
    application:set_env(elector, strategy_module, runtime_high_strategy),
    application:set_env(elector, sync_start, false),
    application:set_env(elector, pre_election_hooks, []),
    application:set_env(elector, startup_hooks_enabled, true),
    application:set_env(elector, post_election_hooks, []).

test_election_delay(_Config) ->
    application:set_env(elector, election_delay, 5000),
    ?assert(config_handler:election_delay() =:= 5000).

test_strategy_module(_Config) ->
    application:set_env(elector, strategy_module, runtime_high_strategy),
    ?assert(config_handler:strategy_module() =:= runtime_high_strategy).

test_sync_start(_Config) ->
    application:set_env(elector, sync_start, true),
    ?assert(config_handler:sync_start() =:= true).

test_pre_election_hooks(_Config) ->
    application:set_env(elector, pre_election_hooks, test_helper:test_hook(pre)),
    elector:elect_sync(),
    Cond1 = config_handler:pre_election_hooks() =:= test_helper:test_hook(pre),
    TriggerType = test_helper:trigger_type(pre),
    Cond2 =
        receive
            TriggerType ->
                true
        after 2000 ->
            false
        end,
    ?assert(Cond1 =:= true andalso Cond2 =:= true).

test_post_election_hooks(_Config) ->
    application:set_env(elector, post_election_hooks, test_helper:test_hook(post)),
    elector:elect_sync(),
    Cond1 = config_handler:post_election_hooks() == test_helper:test_hook(post),
    TriggerType = test_helper:trigger_type(post),
    Cond2 =
        receive
            TriggerType ->
                true
        after 2000 ->
            false
        end,
    ?assert(Cond1 =:= true andalso Cond2 =:= true).

test_startup_hooks_enabled(_Config) ->
    application:set_env(elector, startup_hooks_enabled, false),
    ?assert(config_handler:startup_hooks_enabled() =:= false).
