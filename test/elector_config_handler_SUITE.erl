-module(elector_config_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_election_delay/1, test_strategy_module/1, test_pre_election_hooks/1,
         test_post_election_hooks/1, test_startup_hooks_enabled/1, test_quorum_size/1,
         test_quorum_check/1, test_candidate_node/1, test_hooks_execution/1]).

all() ->
    [test_election_delay,
    test_strategy_module,
    test_pre_election_hooks,
    test_post_election_hooks,
    test_startup_hooks_enabled,
    test_quorum_size,
    test_quorum_check,
    test_candidate_node,
    test_hooks_execution].

init_per_suite(Config) ->
    application:ensure_started(elector),
    Config.

end_per_suite(_Config) ->
    application:set_env(elector, election_delay, 1000),
    application:set_env(elector, strategy_module, elector_rt_high_strategy),
    application:set_env(elector, pre_election_hooks, []),
    application:set_env(elector, startup_hooks_enabled, true),
    application:set_env(elector, post_election_hooks, []),
    application:set_env(elector, candidate_node, true),
    application:set_env(elector, hooks_execution, local),
    application:set_env(elector, quorum_size, 1).

test_election_delay(_Config) ->
    application:set_env(elector, election_delay, 5000),
    5000 = elector_config_handler:election_delay().

test_strategy_module(_Config) ->
    application:set_env(elector, strategy_module, elector_wc_high_strategy),
    elector_wc_high_strategy = elector_config_handler:strategy_module().

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
    Conds = Cond1 =:= true andalso Cond2 =:= true,
    Conds = true.

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
    Conds = Cond1 =:= true andalso Cond2 =:= true,
    true = Conds.

test_startup_hooks_enabled(_Config) ->
    application:set_env(elector, startup_hooks_enabled, false),
    false = elector_config_handler:startup_hooks_enabled().

test_quorum_size(_Config) ->
    application:set_env(elector, quorum_size, 2),
    2 = elector_config_handler:quorum_size().

test_quorum_check(_Config) ->
    application:set_env(elector, quorum_size, 1),
    true = elector_config_handler:quorum_check(),
    application:set_env(elector, quorum_size, 100),
    false = elector_config_handler:quorum_check(),
    Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
    {ok, Peer, _Node} = ?CT_PEER(Paths),
    application:set_env(elector, quorum_size, 2),
    true = elector_config_handler:quorum_check(),
    peer:stop(Peer).

test_candidate_node(_Config) ->
    application:set_env(elector, candidate_node, false),
    false = elector_config_handler:candidate_node(),
    application:set_env(elector, candidate_node, true),
    true = elector_config_handler:candidate_node().

test_hooks_execution(_Config) ->
    application:set_env(elector, hooks_execution, global),
    global = elector_config_handler:hooks_execution(),
    application:set_env(elector, hooks_execution, local),
    local = elector_config_handler:hooks_execution().
