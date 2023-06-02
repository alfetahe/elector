-module(elector_service_SUITE).

-include_lib("common_test/include/ct.hrl").

-behaviour(ct_suite).

-export([all/0]).
-export([test_commission_pid/1, test_hook_exec/1, test_async_call/1]).

all() -> 
    [test_commission_pid, test_hook_exec, test_async_call].

test_commission_pid(_Config) ->
    CommissionPid = elector_service:commission_pid(),
    true = is_pid(CommissionPid),
    true = CommissionPid =:= global:whereis_name(elector_commission).

test_hook_exec(_Config) ->
    Ref = make_ref(),
    elector_service:hook_exec({erlang, is_atom, [any]}, self(), Ref),
    receive
        {hook_executed, Ref} ->
            true
    after 1000 ->
        false
    end.

test_async_call(_Config) ->
    ExampleFun = fun() -> ok end,
    {ok, Peer, Node} = peer_node_setup(),
    Responses = elector_service:async_call(ExampleFun, [node(), Node]),
    true = lists:all(fun({_N, {response, Res}}) -> Res =:= ok end, Responses),
    peer_node_teardown(Peer).

peer_node_setup() ->
    ?CT_PEER(["-pa", code:lib_dir(elector) ++ "/ebin", "-connect_all", "false"]).

peer_node_teardown(Peer) ->
    peer:stop(Peer).