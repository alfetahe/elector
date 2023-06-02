-module(elector_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test_is_leader/1, test_get_leader/1, test_elect_sync/1, test_elect/1,
         test_clear_leader/1]).

-define(NR_OF_NODES, 5).

all() ->
    [test_is_leader, test_get_leader, test_elect_sync, test_elect, test_clear_leader].

init_per_suite(Config) ->
    ConfigSetup = fun() -> config_setup() end,
    Nodes =
        [?CT_PEER(["-pa", code:lib_dir(elector) ++ "/ebin", "-connect_all", "false"])
         || _Nr <- lists:seq(1, ?NR_OF_NODES)],

    [unlink(Peer) || {_, Peer, _Node} <- Nodes],

    ConfigSetup(),
    % Connect all peer nodes to each other an setup config.
    [erpc:call(Node,
               fun() ->
                  ConfigSetup(),
                  [net_kernel:connect_node(PeerNode) || {_, _, PeerNode} <- Nodes]
               end)
     || {_, _Peer, Node} <- Nodes],

    [{nodes, Nodes} | Config].

end_per_suite(Config) ->
    config_cleanup(),
    [peer:stop(Peer) || {_, Peer, _Node} <- ?config(nodes, Config)].

test_is_leader(Config) ->
    {ok, LeaderNode} = elector:elect_sync(),
    Nodes = ?config(nodes, Config),
    Responses =
        [erpc:call(Node,
                   fun() ->
                      {ok, IsLeader} = elector:is_leader(),
                      {Node, IsLeader}
                   end)
         || {_, _, Node} <- [any, any, node() | Nodes]],
    true =
        lists:all(fun ({Node, true}) ->
                          Node =:= LeaderNode;
                      ({Node, false}) ->
                          Node =/= LeaderNode
                  end,
                  Responses).

test_get_leader(Config) ->
    {ok, LeaderNode} = elector:elect_sync(),
    Nodes = ?config(nodes, Config),
    Responses =
        [erpc:call(Node,
                   fun() ->
                      {ok, Ln} = elector:get_leader(),
                      Ln =:= LeaderNode
                   end)
         || {_, _, Node} <- [any, any, node() | Nodes]],
    true = lists:all(fun(Res) -> Res =:= true end, Responses).

test_elect_sync(Config) ->
    Nodes = ?config(nodes, Config),
    {ok, LeaderNode} = elector:elect_sync(),
    Responses =
        [erpc:call(Node,
                   fun() ->
                      {ok, LeaderNode} = elector:elect_sync(),
                      true
                   end)
         || {_, _, Node} <- [any, any, node() | Nodes]],
    true = lists:all(fun(Res) -> Res =:= true end, Responses).

test_elect(Config) ->
    Nodes = ?config(nodes, Config),
    elector:clear_leader(),
    {ok, undefined} = elector:get_leader(),
    ElectSetupFun =
        fun() ->
           application:set_env(elector, election_delay, 0),
           application:set_env(elector,
                               post_election_hooks,
                               [{erlang, send, [self(), election_finished]}]),
           {ok, async_election_started} = elector:elect(),
           receive
               election_finished ->
                   {ok, Node} = elector:get_leader(),
                   true = lists:member(Node, [node() | nodes()])
           after 2000 ->
               false
           end
        end,
    Responses =
        [erpc:call(Node, ElectSetupFun) || {_, _, Node} <- [any, any, node() | Nodes]],
    true = lists:all(fun(Res) -> Res =:= true end, Responses).

test_clear_leader(Config) ->
    Nodes = ?config(nodes, Config),
    elector:elect_sync(),
    {ok, LeaderNode} = elector:get_leader(),
    true = LeaderNode =/= undefined,
    {ok, leader_cleared} = elector:clear_leader(),
    NodesFun =
        fun() ->
           {ok, undefined} = elector:get_leader(),
           true
        end,
    Responses = [erpc:call(Node, NodesFun) || {_, _, Node} <- [any, any, node() | Nodes]],
    true = lists:all(fun(Res) -> Res =:= true end, Responses).

config_setup() ->
    application:ensure_all_started(elector),
    application:set_env(elector, election_delay, 0),
    application:set_env(elector, strategy_module, elector_wc_low_strategy).

config_cleanup() ->
    application:set_env(elector, election_delay, 3000),
    application:set_env(elector, strategy_module, elector_wc_high_strategy),
    application:set_env(elector, post_election_hooks, []).
