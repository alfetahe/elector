%%------------------------------------------------------------------------------
%% @doc Runtime strategy that elects the node with the highest
%% runtime as leader.
%% Runtime is based on CPU time used by the Erlang VM for each thread.
%% @private
%% @end
%%------------------------------------------------------------------------------
-module(elector_rt_high_strategy).

%%------------------------------------------------------------------------------
%% Behaviours
%%------------------------------------------------------------------------------
-behaviour(elector_strategy_behaviour).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([elect/1]).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
%% @doc Starts the election process.
-spec elect(CandidateNodes :: [node()]) -> Leader :: elector_strategy_behaviour:leader().
elect(CandidateNodes) ->
    elector_time_strategy_base:elect(runtime, high, CandidateNodes).
