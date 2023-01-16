-module(rpc_client_SUITE).
-include_lib("eunit/include/eunit.hrl").
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([]).

groups() ->
	[{rpc_client_group, [], [test_rpc_request]}].

all() ->
	[{group, rpc_client_group}].

init_per_group(_GroupName, Config) ->
	application:ensure_start(elector),
	Config.

end_per_group(_GroupName, _Config) ->
	ok.