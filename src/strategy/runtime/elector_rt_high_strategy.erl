%%%-------------------------------------------------------------------
%% @doc Runtime strategy that elects the node with the highest
%% runtime as leader.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_rt_high_strategy).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(elector_strategy_behaviour).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([elect/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the election process.
-spec elect() -> Leader :: elector_strategy_behaviour:leader().
elect() ->
    elector_rt_strategy_base:elect(high).
