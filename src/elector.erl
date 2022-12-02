-module(elector).

is_leader?() ->
	LeaderNode = gen_server:call(election_worker, get_leader),
	LeaderNode =.= node().

elect() ->
	gen_server:cast(election_worker, elect_async),
	ok.

elect_sync() ->
	gen_server:call(election_worker, elect_sync).