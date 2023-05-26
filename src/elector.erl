%%%-------------------------------------------------------------------
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
%%%-------------------------------------------------------------------
-module(elector).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([is_leader/0, elect/0, elect_sync/0, get_leader/0, clear_leader/0,
         clear_leader_sync/0]).

-type leader_node() :: node() | undefined.
-type node_responses() :: {list(), list()}.

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
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
               {ok, election_msg_passed} |
               {error, quorum_size_not_met} |
               {error, election_singleton_not_up}.
elect() ->
    SingletonPid = elector_service:singleton_pid(),
    QuorumRes = elector_config_handler:quorum_check(),
    start_election(async, SingletonPid, QuorumRes).

%% @doc Starts an election synchronously.
-spec elect_sync() ->
                    {ok, leader_node()} |
                    {error, quorum_size_not_met} |
                    {error, election_singleton_not_up}.
elect_sync() ->
    SingletonPid = elector_service:singleton_pid(),
    QuorumRes = elector_config_handler:quorum_check(),
    start_election(sync, SingletonPid, QuorumRes).

%% @doc Clears the leader node and sets it to undefined.
-spec clear_leader() -> {ok, leader_cleared}.
clear_leader() ->
    gen_server:abcast([node() | nodes()], elector_state, clear_leader),
    {ok, leader_cleared}.

%% @doc Clears the leader node and sets it to undefined.
-spec clear_leader_sync() -> {ok, node_responses()}.
clear_leader_sync() ->
    {ok, gen_server:multi_call([node() | nodes()], elector_state, clear_leader)}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
start_election(_Type, _SingletonPid, QuorumCheck) when QuorumCheck =:= false ->
    {error, quorum_size_not_met};
start_election(_Type, SingletonPid, _QuorumCheck) when is_pid(SingletonPid) =:= false ->
    {error, election_singleton_not_up};
start_election(sync, SingletonPid, _QuorumCheck) ->
    LeaderNode = gen_server:call(SingletonPid, start_election),
    {ok, LeaderNode};
start_election(async, SingletonPid, _QuorumCheck) ->
    gen_server:cast(SingletonPid, start_election),
    {ok, election_msg_passed}.
