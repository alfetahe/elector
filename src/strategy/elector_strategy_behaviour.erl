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
-export([elect/0, candidate_nodes/0, check_node_candidate/0]).

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
        elector_config_handler:strategy_module(), elect, [CandidateNodes]
    ).

%% @doc Returns a list of nodes that are eligible to become the leader.
%% @end
-spec candidate_nodes() -> CandidateNodes :: candidate_nodes().
candidate_nodes() ->
    Responses = elector_service:async_call(
        {?MODULE, check_node_candidate, []},
        [node() | nodes()]
    ),
    lists:foldl(
        fun({Node, Response}, Acc) ->
            case Response of
                {response, {ok, true}} ->
                    [Node | Acc];
                {response, {ok, false}} ->
                    Acc;
                {error, _OtherError} ->
                    Acc;
                _ ->
                    % Handle any other unexpected responses - not a candidate
                    Acc
            end
        end,
        [],
        Responses
    ).

%% @doc Checks if the current node is a candidate for election.
%% This function is called remotely on each node in the cluster.
%% @end
-spec check_node_candidate() -> {ok, boolean()}.
check_node_candidate() ->
    try
        %% Check if elector_candidate process exists and is running
        Pid = erlang:whereis(elector_candidate),
        case Pid of
            undefined ->
                %% No elector_candidate process, node is not a candidate
                {ok, false};
            _ when is_pid(Pid) ->
                %% Process exists, check if it's alive and responsive
                case erlang:is_process_alive(Pid) of
                    true ->
                        try
                            %% Use a short timeout to avoid hanging
                            case gen_server:call(Pid, is_candidate_node, 500) of
                                {ok, IsCandidate} when is_boolean(IsCandidate) ->
                                    {ok, IsCandidate};
                                {ok, _Other} ->
                                    {ok, false};
                                _Error ->
                                    {ok, false}
                            end
                        catch
                            _:_ -> {ok, false}
                        end;
                    false ->
                        {ok, false}
                end;
            _ ->
                {ok, false}
        end
    catch
        _Class:_Reason ->
            %% If anything goes wrong, assume not a candidate
            {ok, false}
    end.
