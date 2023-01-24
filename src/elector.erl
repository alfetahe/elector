-module(elector).

-export([is_leader/0, elect/0, elect_sync/0, get_leader/0]).

is_leader() ->
    LeaderNode = gen_server:call(election_worker, get_leader),
    LeaderNode == node().

get_leader() ->
    gen_server:call(election_worker, get_leader).

elect() ->
    gen_server:cast(election_worker, elect_async),
    ok.

elect_sync() ->
    gen_server:call(election_worker, elect_sync).
