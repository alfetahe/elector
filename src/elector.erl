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
-export([is_leader/0, elect/0, elect_sync/0, get_leader/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Returns boolean true this node is the leader or false if not.
-spec is_leader() -> {ok, boolean()} | {error, leader_node_not_set}.
is_leader() ->
    LeaderNode = gen_server:call(elector_worker, get_leader),
    IsLeader = LeaderNode =:= node(),
    if is_atom(LeaderNode) ->
           {ok, IsLeader};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Returns the current leader node's machine name.
-spec get_leader() -> {ok, node()} | {error, leader_node_not_set}.
get_leader() ->
    LeaderNode = gen_server:call(elector_worker, get_leader),
    if is_atom(LeaderNode) ->
           {ok, LeaderNode};
       true ->
           {error, leader_node_not_set}
    end.

%% @doc Starts an election asynchronously.
-spec elect() -> {ok, election_started} | {error, quorum_size_not_met}.
elect() ->
    case elector_config_handler:quorum_check() of
        true ->
            gen_server:cast(electionworker, elect_async),
            {ok, election_started};
        false ->
            {error, quorum_size_not_met}
    end.

%% @doc Starts an election synchronously.
-spec elect_sync() -> {ok, election_finished} | {error, term()}.
elect_sync() ->
    case elector_config_handler:quorum_check() of
        true ->
            Resp = gen_server:call(elector_worker, elect_sync),
            if Resp =:= election_finished ->
                   {ok, election_finished};
               true ->
                   {error, Resp}
            end;
        false ->
            {error, quorum_size_not_met}
    end.
