-module(elector_config_handler_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1]).
-export([test_election_delay/1, test_strategy_module/1, test_pre_election_hooks/1,
         test_post_election_hooks/1, test_quorum_size/1, test_quorum_check/1,
         test_candidate_node/1, test_hooks_execution/1, test_automatic_elections/1,
         test_add_pre_election_hook/1, test_add_post_election_hook/1, test_rem_pre_election_hook/1,
         test_rem_post_election_hook/1]).

all() ->
    [test_election_delay,
     test_strategy_module,
     test_pre_election_hooks,
     test_post_election_hooks,
     test_quorum_size,
     test_quorum_check,
     test_candidate_node,
     test_hooks_execution,
     test_automatic_elections,
     test_add_pre_election_hook,
     test_add_post_election_hook,
     test_rem_pre_election_hook,
     test_rem_post_election_hook].

init_per_suite(Config) ->
    application:ensure_started(elector),
    Config.

end_per_suite(_Config) ->
    application:set_env(elector, election_delay, 1000),
    application:set_env(elector, strategy_module, elector_rt_high_strategy),
    application:set_env(elector, pre_election_hooks, []),
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

test_quorum_size(_Config) ->
    application:set_env(elector, quorum_size, 5),
    5 = elector_config_handler:quorum_size().

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

test_automatic_elections(_Config) ->
    true = elector_config_handler:automatic_elections(),
    application:set_env(elector, automatic_elections, false),
    false = elector_config_handler:automatic_elections(),
    application:set_env(elector, automatic_elections, true),
    true = elector_config_handler:automatic_elections().

test_add_pre_election_hook(_Config) ->
    application:set_env(elector, pre_election_hooks, []),
    elector_config_handler:add_pre_election_hook(pre_module1, test_func1, [1]),
    elector_config_handler:add_pre_election_hook(pre_module2, test_func2, [2]),
    {ok, Hooks} = application:get_env(elector, pre_election_hooks),
    [{pre_module2, test_func2, [2]}, {pre_module1, test_func1, [1]}] = Hooks.

test_add_post_election_hook(_Config) ->
    application:set_env(elector, post_election_hooks, []),
    elector_config_handler:add_post_election_hook(post_module1, test_func1, [1]),
    elector_config_handler:add_post_election_hook(post_module2, test_func2, [2]),
    {ok, Hooks} = application:get_env(elector, post_election_hooks),
    [{post_module2, test_func2, [2]}, {post_module1, test_func1, [1]}] = Hooks.

test_rem_pre_election_hook(_Config) ->
    TestHooks = elector_test_helper:test_hook(pre),
    {M, F, A} = lists:nth(1, TestHooks),
    application:set_env(elector, pre_election_hooks, TestHooks),
    {ok, TestHooks} = application:get_env(elector, pre_election_hooks),
    elector_config_handler:rem_pre_election_hook(M, F, A),
    {ok, []} = application:get_env(elector, pre_election_hooks).

test_rem_post_election_hook(_Config) ->
    TestHooks = elector_test_helper:test_hook(post),
    {M, F, A} = lists:nth(1, TestHooks),
    application:set_env(elector, post_election_hooks, TestHooks),
    {ok, TestHooks} = application:get_env(elector, post_election_hooks),
    elector_config_handler:rem_post_election_hook(M, F, A),
    {ok, []} = application:get_env(elector, post_election_hooks).
