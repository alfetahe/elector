-module(elector).

is_leader?() ->
	LeaderNode = gen_server:call(election_worker, get_leader),
	LeaderNode =.= node().