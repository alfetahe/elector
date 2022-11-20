-module(runtime_strategy).

-behaviour(strategy_behaviour).

-export([nodes_call/1]).

nodes_call(_Nodes_list) ->
    ok.