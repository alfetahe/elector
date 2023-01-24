-module(rpc_client_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([test_call/1, test_nodes/1]).

all() ->
    [test_nodes, test_call].

init_per_testcase(_TestCase, Config) ->
    Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
    {ok, Peer, Node} = ?CT_PEER(Paths),
    [{peer_node, {Peer, Node}} | Config].

end_per_testcase(_TestCase, Config) ->
    {Peer, _Node} = ?config(peer_node, Config),
    peer:stop(Peer).

test_nodes(Config) ->
    {_Peer, Node} = ?config(peer_node, Config),
    ?assert(rpc_client:nodes() =:= [node(), Node]),
    {ok, NewPeer, NewNode} = ?CT_PEER(),
    ?assert(rpc_client:nodes() =:= [node(), Node, NewNode]),
    peer:stop(NewPeer).

test_call(Config) ->
    {_Peer, Node} = ?config(peer_node, Config),
    Resp = rpc_client:call(Node, test_helper, ping, []),
    ?assert(Resp =:= pong).
