-module(rpc_client).

-export([call/4, connected_nodes/0]).

call(Node, Module, Func, Args) ->
    rpc:call(Node, Module, Func, Args).

connected_nodes() ->
    erlang:nodes().
