-module(election_worker_SUITE).
-include_lib("common_test/include/ct.hrl").
-export([groups/0, all/0, init_per_group/2, end_per_group/2]).
-export([test_node_connecting/1]).

groups() ->
	[{election_worker_group, [], [test_node_connecting]}].

all() ->
	[{group, election_worker_group}].

init_per_group(_GroupName, Config) ->
	Config.

end_per_group(_GroupName, _Config) ->
	ok.

test_node_connecting(_Config) ->
    % Connect child node to parent node. 
	ok.

