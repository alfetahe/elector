%%%-------------------------------------------------------------------
%% @doc This is the main API module.
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
%% @doc Returns boolean wether this node is the leader or not.
-spec is_leader() -> boolean().
is_leader() ->
    LeaderNode = gen_server:call(election_worker, get_leader),
    LeaderNode == node().

%% @doc Returns the current leader node's machine name.
-spec get_leader() -> node().    
get_leader() ->
    gen_server:call(election_worker, get_leader).

%% @doc Starts an election asynchronously.
-spec elect() -> ok.
elect() ->
    gen_server:cast(election_worker, elect_async),
    ok.

%% @doc Starts an election synchronously.
-spec elect_sync() -> election_finished.
elect_sync() ->
    gen_server:call(election_worker, elect_sync).
