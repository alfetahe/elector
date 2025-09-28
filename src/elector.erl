%%%-----------------------------------------------------------------------------
%% @doc Elector - Distributed Leader Election for Erlang/OTP
%%
%% This is the main API module for the Elector application. Elector automatically
%% detects all nodes in a distributed Erlang cluster and chooses a leader node.
%% Elections are started automatically when the application starts or when nodes
%% join/leave the cluster.
%%
%% ## Quick Start
%%
%% The `elector' application must be started before calling any functions:
%%
%% ```
%% % Start elector application
%% ok = application:start(elector).
%%
%% % Check if current node is leader
%% {ok, IsLeader} = elector:is_leader().
%%
%% % Get the current leader node
%% {ok, LeaderNode} = elector:get_leader().
%%
%% % Manually trigger an election
%% {ok, election_started} = elector:elect().
%% '''
%%
%% ## Automatic Elections
%%
%% When `elector' is started, it handles elections automatically:
%% - On application startup
%% - When new nodes join the cluster
%% - When existing nodes leave the cluster
%%
%% You can also trigger elections manually using `elect/0' or `elect_sync/0'.
%%
%% For detailed configuration and setup instructions, see the
%% [Getting Started guide](readme.html).
%% @end
%%%-----------------------------------------------------------------------------
-module(elector).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([is_leader/0, elect/0, elect_sync/0, get_leader/0, clear_leader/0]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type leader_node() :: node() | undefined.

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
%% @doc Checks if the current node is the elected leader.
%%
%% Returns `{ok, true}' if this node is currently the leader,
%% `{ok, false}' if another node is the leader, or an error if
%% no leader has been elected yet.
%%
%% ## Examples
%%
%% ```
%% % Check leadership status
%% case elector:is_leader() of
%%     {ok, true} ->
%%         io:format("This node is the leader~n");
%%     {ok, false} ->
%%         io:format("This node is not the leader~n");
%%     {error, leader_node_not_set} ->
%%         io:format("No leader elected yet~n")
%% end.
%% '''
%% @end
-spec is_leader() -> {ok, boolean()} | {error, leader_node_not_set}.
is_leader() ->
    LeaderNode = gen_server:call(elector_state, get_leader),
    IsLeader = LeaderNode =:= node(),
    if is_atom(LeaderNode) ->
           {ok, IsLeader};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Returns the current leader node's name.
%%
%% ## Examples
%%
%% ```
%% case elector:get_leader() of
%%     {ok, LeaderNode} ->
%%         io:format("Leader is: ~p~n", [LeaderNode]);
%%     {error, leader_node_not_set} ->
%%         io:format("No leader elected yet~n")
%% end.
%% '''
%% @end
-spec get_leader() -> {ok, node()} | {error, leader_node_not_set}.
get_leader() ->
    LeaderNode = gen_server:call(elector_state, get_leader),
    if is_atom(LeaderNode) ->
           {ok, LeaderNode};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Starts an election process asynchronously.
%%
%% This function triggers a new leader election across the cluster.
%% The election runs in the background and doesn't block the caller.
%%
%% ## Returns
%%
%% - `{ok, async_election_started}' - Election started successfully
%% - `{error, quorum_size_not_met}' - Not enough nodes in cluster
%% - `{error, election_commission_not_up}' - Election service unavailable
%%
%% ## Examples
%%
%% ```
%% case elector:elect() of
%%     {ok, async_election_started} ->
%%         io:format("Election started~n");
%%     {error, Reason} ->
%%         io:format("Election failed: ~p~n", [Reason])
%% end.
%% '''
%% @end
-spec elect() ->
               {ok, async_election_started} |
               {error, quorum_size_not_met} |
               {error, election_commission_not_up}.
elect() ->
    CommissionPid = elector_service:commission_pid(),
    QuorumRes = elector_config_handler:quorum_check(),
    start_election(async, CommissionPid, QuorumRes).

%% @doc Starts an election synchronously.
-spec elect_sync() ->
                    {ok, leader_node()} |
                    {error, quorum_size_not_met} |
                    {error, election_commission_not_up}.
elect_sync() ->
    CommissionPid = elector_service:commission_pid(),
    QuorumRes = elector_config_handler:quorum_check(),
    start_election(sync, CommissionPid, QuorumRes).

%% @doc Clears the leader node and sets it to undefined.
-spec clear_leader() -> {ok, leader_cleared}.
clear_leader() ->
    gen_server:abcast([node() | nodes()], elector_state, clear_leader),
    {ok, leader_cleared}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
start_election(_Type, _CommissionPid, QuorumCheck) when QuorumCheck =:= false ->
    {error, quorum_size_not_met};
start_election(_Type, CommissionPid, _QuorumCheck) when is_pid(CommissionPid) =:= false ->
    {error, election_commission_not_up};
start_election(sync, CommissionPid, _QuorumCheck) ->
    LeaderNode = gen_server:call(CommissionPid, start_election),
    {ok, LeaderNode};
start_election(async, CommissionPid, _QuorumCheck) ->
    gen_server:cast(CommissionPid, start_election),
    {ok, async_election_started}.
