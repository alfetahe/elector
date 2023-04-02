-module(elector_SUITE).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([test_is_leader/1, test_get_leader/1, test_elect_sync/1, test_elect/1,
         test_clear_leader/1]).

all() ->
    [test_is_leader, test_get_leader, test_elect_sync, test_elect, test_clear_leader].

init_per_suite(Config) ->
    application:ensure_all_started(elector),
    application:set_env(elector, election_delay, 0),
    application:set_env(elector, strategy_module, elector_rt_low_strategy),
    Config.

end_per_suite(_Config) ->
    application:set_env(elector, election_delay, 3000),
    application:set_env(elector, strategy_module, elector_rt_high_strategy),
    application:set_env(elector, post_election_hooks, []),
    ok.

test_is_leader(_Config) ->
    ?assert(elector:is_leader() =:= {ok, true}),
    {ok, Peer, _Node} = peer_node_setup(),
    elector:elect_sync(),
    ?assert(elector:is_leader() =:= {ok, false}),
    peer_node_teardown(Peer).

test_get_leader(_Config) ->
    ?assert(elector:get_leader() =:= {ok, node()}),
    {ok, Peer, Node} = peer_node_setup(),
    elector:elect_sync(),
    ?assert(elector:get_leader() =:= {ok, Node}),
    peer_node_teardown(Peer).

test_elect_sync(_Config) ->
    ?assert(elector:elect_sync() =:= {ok, election_finished}),
    ?assert(elector:get_leader() =:= {ok, node()}).

test_elect(_Config) ->
    elector:clear_leader(),
    ?assert(elector:get_leader() =:= {ok, undefined}),
    application:set_env(elector, election_delay, 0),
    application:set_env(elector,
                        post_election_hooks,
                        [{erlang, send, [self(), election_finished]}]),
    ?assert(elector:elect() =:= {ok, election_started}),
    receive
        election_finished ->
            ?assert(elector:get_leader() =:= {ok, node()})
    after 2000 ->
        ?assert(false)
    end.

test_clear_leader(_Config) ->
    elector:elect_sync(),
    ?assert(elector:get_leader() =:= {ok, node()}),
    elector:clear_leader(),
    ?assert(elector:get_leader() =:= {ok, undefined}).

peer_node_setup() ->
    Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
    ?CT_PEER(Paths).

peer_node_teardown(Peer) ->
    peer:stop(Peer).
