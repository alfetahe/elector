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
-export([is_leader/0, elect/0, elect_sync/0, get_leader/0, clear_leader/0]).

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
-spec elect() -> {ok, election_started} | {error, quorum_size_not_met}.
elect() ->
    QuorumRes = elector_config_handler:quorum_check(),
    elect_with_quorum_check(QuorumRes, async).

%% @doc Starts an election synchronously.
-spec elect_sync() -> {ok, election_finished} | {error, term()}.
elect_sync() ->
    QuorumRes = elector_config_handler:quorum_check(),
    elect_with_quorum_check(QuorumRes, sync).

%% @doc Clears the leader node and sets it to undefined.
-spec clear_leader() -> {ok, leader_cleared}.
clear_leader() ->
    gen_server:call(elector_state, clear_leader).

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
elect_with_quorum_check(QuorumCheck, ElectType) when QuorumCheck =:= true ->
    case ElectType of
        sync ->
            Resp =
                gen_server:multi_call(
                    elector_rpc_client:nodes(), elector_state, elect_sync),
            handle_sync_elect_resp(Resp);
        async ->
            gen_server:abcast(
                elector_rpc_client:nodes(), elector_state, elect_async),
            {ok, election_started}
    end;
elect_with_quorum_check(_QuorumCheck, _ElectType) ->
    {error, quorum_size_not_met}.

%% @private
handle_sync_elect_resp({SuccResp, BadNodes}) ->
    case BadNodes of
        [] ->
            case fold_responses(SuccResp) of
                [] ->
                    {ok, election_finished};
                Res ->
                    {error, {elect_failed_nodes, Res}}
            end;
        _ ->
            {error, {nodes_multicall_failed, BadNodes}}
    end.

%% @private
fold_responses(SuccResp) ->
    lists:foldl(fun({Node, Resp}, Acc) ->
                   case Resp of
                       election_finished ->
                           Acc;
                       _ ->
                           [{Node, Resp} | Acc]
                   end
                end,
                [],
                SuccResp).
