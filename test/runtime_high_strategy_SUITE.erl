-module(runtime_high_strategy_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(ct_suite).
-export([all/0]).
-export([init_per_testcase/2, end_per_testcase/2]).
-export([test_elect/1]).

all() ->
	[test_elect].

init_per_testcase(_TestCase, Config) ->
	Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
	{ok, Peer, Node} = ?CT_PEER(Paths),
	[{peer_node, {Peer, Node}} | Config].

end_per_testcase(_TestCase, Config) ->
	{Peer, _Node} = ?config(peer_node, Config),
	peer:stop(Peer).

test_elect(_Config) ->
	SelectedNode = runtime_high_strategy:elect(),
	?assert(SelectedNode =:= node()).
