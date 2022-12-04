-module(election_worker_SUITE).

-include_lib("eunit/include/eunit.hrl").

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

all() ->
    [basic_test, basic_fail_test].

basic_test(_Config) ->
    ?assert(1 + 1 =:= 2).

basic_fail_test(_Config) ->
    ?assert(1 =:= 2).