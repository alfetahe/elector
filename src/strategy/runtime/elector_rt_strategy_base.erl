%%%-------------------------------------------------------------------
%% @doc Runtime strategy base functionality module used by the
%% high and low runtime strategies.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_rt_strategy_base).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([elect/1, host_node_runtime/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the election process.
-spec elect(Type :: high | low) -> Leader :: elector_strategy_behaviour:leader().
elect(Type) ->
    Runtimes = iterate_runtimes(elector_rpc_client:nodes(), #{}),
    choose_leader(Runtimes, Type).

%% @doc Returns the runtime of the host node.
-spec host_node_runtime() -> Runtime :: integer().
host_node_runtime() ->
    {Runtime, _TimeSinceLastCall} = erlang:statistics(runtime),
    Runtime.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @private
choose_leader(Runtimes, Type) ->
    CompareFn =
        fun(Node, Runtime, {AccNode, AccRuntime} = Acc) ->
           if AccNode == nil
              orelse Type =:= high andalso Runtime > AccRuntime
              orelse Type =:= low andalso Runtime < AccRuntime
              orelse Runtime =:= AccRuntime andalso Node > AccNode ->
                  {Node, Runtime};
              true ->
                  Acc
           end
        end,

    {Node, _Runtime} = maps:fold(CompareFn, {nil, 0}, Runtimes),
    Node.

%% @private
iterate_runtimes([Node], Runtimes) ->
    {Runtime, _} = elector_rpc_client:call(Node, erlang, statistics, [runtime]),
    maps:put(Node, Runtime, Runtimes);
iterate_runtimes([Node | Nbody], Runtimes) ->
    {Runtime, _} = elector_rpc_client:call(Node, erlang, statistics, [runtime]),
    iterate_runtimes(Nbody, maps:put(Node, Runtime, Runtimes));
iterate_runtimes([], Runtimes) ->
    Runtimes.
