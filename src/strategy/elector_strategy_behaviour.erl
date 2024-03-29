%%%-----------------------------------------------------------------------------
%% @doc Defines the base behaviour for election strategies.
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_strategy_behaviour).

%%------------------------------------------------------------------------------
%% Callbacks
%%------------------------------------------------------------------------------
-callback elect(CandidateNodes :: candidate_nodes()) -> Leader :: leader().

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([elect/0, candidate_nodes/0]).

%%------------------------------------------------------------------------------
%% Type definitions
%%------------------------------------------------------------------------------
-type leader() :: node().
-type candidate_nodes() :: [node()].

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
%% @doc Starts the election process by triggering the strategy
%% modules elect() function.
%% The election implementation should only contain the logic for
%% selecting the leader node and returning the leader node name.
%% The elect/0 function is triggered on all nodes automatically
%% by the elector application. This means the strategy implementation
%% does not have to worry about starting the election on all nodes.
%% @end
-spec elect() -> Leader :: leader().
elect() ->
    CandidateNodes = candidate_nodes(),
    erlang:apply(
        elector_config_handler:strategy_module(), elect, [CandidateNodes]).

%% @doc Returns a list of nodes that are eligible to become the leader.
%% @end
-spec candidate_nodes() -> CandidateNodes :: candidate_nodes().
candidate_nodes() ->
    NodeCandidationFun =
        fun() ->
           Pid = erlang:whereis(elector_candidate),
           case Pid of
               undefined ->
                   {ok, false};
               _ ->
                   gen_server:call(Pid, is_candidate_node)
           end
        end,
    Responses = elector_service:async_call(NodeCandidationFun, [node() | nodes()]),
    [Node || {Node, {response, {ok, IsCandidate}}} <- Responses, IsCandidate =:= true].
