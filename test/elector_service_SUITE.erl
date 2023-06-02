-module(elector_service_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([test_commission_pid/1]).

all() -> 
    [test_commission_pid].

test_commission_pid(_Config) ->
    CommissionPid = elector_service:commission_pid(),
    true = is_pid(CommissionPid),
    true = CommissionPid =:= global:whereis_name(elector_commission).