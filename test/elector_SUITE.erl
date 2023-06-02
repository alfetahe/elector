-module(elector_SUITE).

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
    {ok, true} = elector:is_leader(),
    {ok, Peer, _Node} = peer_node_setup(),
    elector:elect_sync(),
    {ok, false} = elector:is_leader(),
    peer_node_teardown(Peer).

test_get_leader(_Config) ->
    LocalNode = node(),
    {ok, LocalNode} = elector:get_leader(),
    {ok, Peer, Node} = peer_node_setup(),
    elector:elect_sync(),
    {ok, Node} = elector:get_leader(),
    peer_node_teardown(Peer).

test_elect_sync(_Config) ->
    Node = node(),
    {ok, election_finished} = elector:elect_sync(),
    {ok, Node} = elector:get_leader().

test_elect(_Config) ->
    elector:clear_leader(),
    {ok, undefined} = elector:get_leader(),
    application:set_env(elector, election_delay, 0),
    application:set_env(elector,
                        post_election_hooks,
                        [{erlang, send, [self(), election_finished]}]),
    {ok, election_started} = elector:elect(),
    receive
        election_finished ->
            Node = node(),
            {ok, Node} = elector:get_leader()
    after 2000 ->
        throw(timeout)
    end.

test_clear_leader(_Config) ->
    elector:elect_sync(),
    Node = node(),
    {ok, Node} = elector:get_leader(),
    elector:clear_leader(),
    {ok, undefined} = elector:get_leader().

peer_node_setup() ->
    Paths = lists:append([["-pa", code:lib_dir(elector) ++ "/ebin"]]),
    ?CT_PEER(Paths).

peer_node_teardown(Peer) ->
    peer:stop(Peer).
