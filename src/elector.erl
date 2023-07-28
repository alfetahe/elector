%%%-----------------------------------------------------------------------------
%% @doc This is the main API module.
%%
%% This module can be used to manually start new election or to check
%% if the current node is the leader. The `elector' application has
%% to be started before calling any of these functions. To start the
%% `elector' add it to your supervision tree or start it manually.
%%
%% When `elector' is started it will handle the elections automatically
%% startup and when new node joins/leaves erlang the cluster.
%% The elections are started automatically when new node joins the
%% cluster or old one leaves. It is possible to start an election
%% manually by calling `elector:elect/0' or `elect_sync/0'.
%%
%% See the README.md file for more information.
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
%% @doc Returns boolean true this node is the leader or false if not.
-spec is_leader() -> {ok, boolean()} | {error, leader_node_not_set}.
is_leader() ->
    LeaderNode = gen_server:call(elector_state, get_leader),
    IsLeader = LeaderNode =:= node(),
    if is_atom(LeaderNode) ->
           {ok, IsLeader};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Returns the current leader node's machine name.
-spec get_leader() -> {ok, node()} | {error, leader_node_not_set}.
get_leader() ->
    LeaderNode = gen_server:call(elector_state, get_leader),
    if is_atom(LeaderNode) ->
           {ok, LeaderNode};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Starts an election asynchronously.
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
