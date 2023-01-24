-module(rpc_client).

-export([call/3, call/4, nodes/0]).

call(Node, Module, Func) ->
    rpc:call(Node, Module, Func, []).

call(Node, Module, Func, Args) ->
    rpc:call(Node, Module, Func, Args).

nodes() ->
    [node() | erlang:nodes()].
