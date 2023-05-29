%%%-------------------------------------------------------------------
%% @doc Runtime strategy that elects the node with the lowest
%% runtime as leader.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_wc_low_strategy).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(elector_strategy_behaviour).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([elect/1]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the election process.
-spec elect(CandidateNodes :: [node()]) -> Leader :: elector_strategy_behaviour:leader().
elect(CandidateNodes) ->
    elector_time_strategy_base:elect(wall_clock, low, CandidateNodes).
