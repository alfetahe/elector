-module(elector_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([test_commission_pid/1, test_hook_exec/1, test_async_call/1, test_iterate_hooks/1,
         test_setup_election_local/1, test_setup_election_global/1,
         test_setup_election_nohooks/1]).

all() ->
    [test_commission_pid,
     test_hook_exec,
     test_async_call,
     test_iterate_hooks,
     test_setup_election_local,
     test_setup_election_global,
     test_setup_election_nohooks].

test_commission_pid(_Config) ->
    CommissionPid = elector_service:commission_pid(),
    true = is_pid(CommissionPid),
    true = CommissionPid =:= global:whereis_name(elector_commission).

test_hook_exec(_Config) ->
    Ref = make_ref(),
    elector_service:hook_exec({erlang, is_atom, [any]}, self(), Ref),
    receive
        {hook_executed, Ref} ->
            true
    after 1000 ->
        false
    end.

test_async_call(_Config) ->
    ExampleFun = fun() -> ok end,
    {ok, Peer, Node} = peer_node_setup(),
    Responses = elector_service:async_call(ExampleFun, [node(), Node]),
    true = lists:all(fun({_N, {response, Res}}) -> Res =:= ok end, Responses),
    peer_node_teardown(Peer).

test_iterate_hooks(_Config) ->
    Pid = self(),
    ok = elector_service:iterate_hooks([{erlang, send, [Pid, test]}], false),
    false =
        receive
            test ->
                true
        after 0 ->
            false
        end,
    ok = elector_service:iterate_hooks([{erlang, send, [Pid, test]}], true),
    true =
        receive
            test ->
                true
        after 0 ->
            false
        end.

test_setup_election_local(_Config) ->
    Pid = self(),
    {ok, Peer, PeerNode} = peer_node_setup(),
    NodeSetupFun =
        fun() ->
           application:ensure_started(elector),
           application:set_env(elector, automatic_elections, false),
           application:set_env(elector,
                               pre_election_hooks,
                               [{erlang, send, [Pid, pre_election_local]}]),
           application:set_env(elector,
                               post_election_hooks,
                               [{erlang, send, [Pid, post_election_local]}]),
           application:set_env(elector, hooks_execution, local)
        end,
    [erpc:call(Node, NodeSetupFun) || Node <- [node(), PeerNode]],

    LeaderNode = elector_service:setup_election(#{run_hooks => true}),

    NodeCheckupFun =
        fun() ->
           {ok, LeaderNode} = elector:get_leader(),
           true
        end,

    Responses = [erpc:call(Node, NodeCheckupFun) || Node <- [node(), PeerNode]],
    true = lists:all(fun(Resp) -> Resp =:= true end, Responses),

    HooksTestFun =
        fun(MatchType) ->
           MatchType =
               receive
                   pre_election_local ->
                       true
               after 0 ->
                   false
               end,
           MatchType =
               receive
                   post_election_local ->
                       true
               after 0 ->
                   false
               end,
           true
        end,
    HookResponses = [HooksTestFun(MatchType) || MatchType <- [true, false]],
    true = lists:all(fun(Resp) -> Resp =:= true end, HookResponses),

    peer_node_teardown(Peer).

test_setup_election_global(_Config) ->
    Pid = self(),
    {ok, Peer, PeerNode} = peer_node_setup(),
    NodeSetupFun =
        fun() ->
           application:ensure_started(elector),
           application:set_env(elector, automatic_elections, false),
           application:set_env(elector,
                               pre_election_hooks,
                               [{erlang, send, [Pid, pre_election_global]}]),
           application:set_env(elector,
                               post_election_hooks,
                               [{erlang, send, [Pid, post_election_global]}]),
           application:set_env(elector, hooks_execution, global)
        end,
    [erpc:call(Node, NodeSetupFun) || Node <- [node(), PeerNode]],

    LeaderNode = elector_service:setup_election(#{run_hooks => true}),

    NodeCheckupFun =
        fun() ->
           {ok, LeaderNode} = elector:get_leader(),
           true
        end,

    Responses = [erpc:call(Node, NodeCheckupFun) || Node <- [node(), PeerNode]],
    true = lists:all(fun(Resp) -> Resp =:= true end, Responses),

    HooksTestFun =
        fun(MatchType) ->
           MatchType =
               receive
                   pre_election_global ->
                       true
               after 0 ->
                   false
               end,
           MatchType =
               receive
                   post_election_global ->
                       true
               after 0 ->
                   false
               end,
           true
        end,
    HookResponses = [HooksTestFun(MatchType) || MatchType <- [true, true]],
    true = lists:all(fun(Resp) -> Resp =:= true end, HookResponses),

    peer_node_teardown(Peer).

test_setup_election_nohooks(_Config) ->
    Pid = self(),
    {ok, Peer, PeerNode} = peer_node_setup(),
    NodeSetupFun =
        fun() ->
           application:ensure_started(elector),
           application:set_env(elector, automatic_elections, false),
           application:set_env(elector,
                               pre_election_hooks,
                               [{erlang, send, [Pid, pre_election_nohooks]}]),
           application:set_env(elector,
                               post_election_hooks,
                               [{erlang, send, [Pid, post_election_nohooks]}]),
           application:set_env(elector, hooks_execution, global)
        end,
    [erpc:call(Node, NodeSetupFun) || Node <- [node(), PeerNode]],

    LeaderNode = elector_service:setup_election(#{run_hooks => false}),

    NodeCheckupFun =
        fun() ->
           {ok, LeaderNode} = elector:get_leader(),
           true
        end,

    Responses = [erpc:call(Node, NodeCheckupFun) || Node <- [node(), PeerNode]],
    true = lists:all(fun(Resp) -> Resp =:= true end, Responses),

    HooksTestFun =
        fun(MatchType) ->
           MatchType =
               receive
                   pre_election_nohooks ->
                       true
               after 0 ->
                   false
               end,
           MatchType =
               receive
                   post_election_nohooks ->
                       true
               after 0 ->
                   false
               end,
           true
        end,
    HookResponses = [HooksTestFun(MatchType) || MatchType <- [false, false]],
    true = lists:all(fun(Resp) -> Resp =:= true end, HookResponses),

    peer_node_teardown(Peer).

peer_node_setup() ->
    ?CT_PEER(["-pa", code:lib_dir(elector) ++ "/ebin", "-connect_all", "false"]).

peer_node_teardown(Peer) ->
    peer:stop(Peer).
