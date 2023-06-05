-module(elector_wc_high_strategy_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([test_elect/1]).

all() ->
    [test_elect].

test_elect(_Config) ->
    {ok, Peer, PeerNode} = ?CT_PEER(["-pa", code:lib_dir(elector) ++ "/ebin", "-connect_all", "false"]),
        NodeSetupFun =
        fun() ->
            application:set_env(elector, automatic_elections, false),
            application:ensure_started(elector)
        end,
    [erpc:call(Node, NodeSetupFun) || Node <- [node(), PeerNode]],

    CandidateNodes = elector_strategy_behaviour:candidate_nodes(),
    SelectedNode = elector_wc_high_strategy:elect(CandidateNodes),
    SelectedNode = node(),
    peer:stop(Peer).
