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
    Responses = elector_service:async_call(
        fun elector_service:check_candidate_node/0,
        [node() | nodes()]
    ),
    lists:foldl(
        fun({Node, Response}, Acc) ->
            case Response of
                {response, {ok, true}} ->
                    [Node | Acc];
                {response, {ok, false}} ->
                    Acc;
                {response, {error, _Reason}} ->
                    % Log error but don't include node in candidates
                    Acc;
                {error, {erpc, _Reason}} ->
                    % Handle erpc errors (node down, timeout, etc.)
                    % Log error but don't include node in candidates
                    Acc;
                _ ->
                    % Handle any other unexpected responses
                    Acc
            end
        end,
        [],
        Responses
    ).
