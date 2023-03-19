%%%-------------------------------------------------------------------
%% @doc Rpc client module used for making distributed requests.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_rpc_client).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([call/3, call/4, nodes/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Call a function on a remote node.
-spec call(node(), module(), atom()) -> any().
call(Node, Module, Func) ->
    rpc:call(Node, Module, Func, []).

%% @doc Call a function on a remote node.
-spec call(node(), module(), atom(), list()) -> any().
call(Node, Module, Func, Args) ->
    rpc:call(Node, Module, Func, Args).

%% @doc Return a list of all nodes in the cluster including self.
-spec nodes() -> [node()].
nodes() ->
    [node() | erlang:nodes()].
