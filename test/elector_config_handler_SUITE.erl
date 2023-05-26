-module(elector_config_handler_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_election_delay/1, test_strategy_module/1, test_pre_election_hooks/1,
         test_post_election_hooks/1, test_startup_hooks_enabled/1, test_quorum_size/1,
         test_quorum_check/1]).

groups() ->
    [{elector_config_handler_group,
      [],
      [test_election_delay,
       test_strategy_module,
       test_pre_election_hooks,
       test_post_election_hooks,
       test_startup_hooks_enabled,
       test_quorum_size,
       test_quorum_check]}].

all() ->
    [{group, elector_config_handler_group}].

init_per_group(_GroupName, Config) ->
    application:ensure_started(elector),
    Config.

end_per_group(_GroupName, _Config) ->
    application:set_env(elector, election_delay, 1000),
    application:set_env(elector, strategy_module, elector_rt_high_strategy),
    application:set_env(elector, pre_election_hooks, []),
    application:set_env(elector, startup_hooks_enabled, true),
    application:set_env(elector, post_election_hooks, []),
    application:set_env(elector, quorum_size, 1).

test_election_delay(_Config) ->
    application:set_env(elector, election_delay, 5000),
    ?assert(elector_config_handler:election_delay() =:= 5000).

test_strategy_module(_Config) ->
    application:set_env(elector, strategy_module, elector_rt_high_strategy),
    ?assert(elector_config_handler:strategy_module() =:= elector_rt_high_strategy).

test_pre_election_hooks(_Config) ->
    application:set_env(elector, pre_election_hooks, elector_test_helper:test_hook(pre)),
    elector:elect_sync(),
    Cond1 =
        elector_config_handler:pre_election_hooks() =:= elector_test_helper:test_hook(pre),
    TriggerType = elector_test_helper:trigger_type(pre),
    Cond2 =
        receive
            TriggerType ->
                true
        after 2000 ->
            false
        end,
    ?assert(Cond1 =:= true andalso Cond2 =:= true).

test_post_election_hooks(_Config) ->
    application:set_env(elector, post_election_hooks, elector_test_helper:test_hook(post)),
    elector:elect_sync(),
    Cond1 =
        elector_config_handler:post_election_hooks() == elector_test_helper:test_hook(post),
    TriggerType = elector_test_helper:trigger_type(post),
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
    ?assert(elector_config_handler:startup_hooks_enabled() =:= false).

test_quorum_size(_Config) ->
    application:set_env(elector, quorum_size, 2),
    ?assert(elector_config_handler:quorum_size() =:= 2).

test_quorum_check(_Config) ->
    application:set_env(elector, quorum_size, 1),
    ?assert(elector_config_handler:quorum_check()),
    application:set_env(elector, quorum_size, 2),
    ?assert(elector_config_handler:quorum_check() =:= false),
    Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
    {ok, Peer, _Node} = ?CT_PEER(Paths),
    ?assert(elector_config_handler:quorum_check()),
    peer:stop(Peer).
