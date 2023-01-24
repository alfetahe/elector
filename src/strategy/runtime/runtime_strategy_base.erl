-module(runtime_strategy_base).

-export([elect/1, host_node_runtime/0]).

elect(Type) ->
    Runtimes = iterate_runtimes(rpc_client:nodes(), #{}),
    choose_leader(Runtimes, Type).

host_node_runtime() ->
    {Runtime, _TimeSinceLastCall} = erlang:statistics(runtime),
    Runtime.

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

iterate_runtimes([Node], Runtimes) ->
    {Runtime, _} = rpc_client:call(Node, erlang, statistics, [runtime]),
    maps:put(Node, Runtime, Runtimes);

iterate_runtimes([Node | Nbody], Runtimes) ->
    {Runtime, _} = rpc_client:call(Node, erlang, statistics, [runtime]),
    iterate_runtimes(Nbody, maps:put(Node, Runtime, Runtimes));

iterate_runtimes([], Runtimes) ->
    Runtimes.
