%%%-----------------------------------------------------------------------------
%% @doc Module responsible for handling configuration.
%% These settings can be altered with custom settings in the
%% application environment.
%% @private
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_config_handler).

-include("elector_header.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([
    election_delay/0,
    strategy_module/0,
    pre_election_hooks/0,
    post_election_hooks/0,
    quorum_size/0,
    quorum_check/0,
    candidate_node/0,
    hooks_execution/0,
    automatic_elections/0,
    add_pre_election_hook/3,
    add_post_election_hook/3,
    rem_pre_election_hook/3,
    rem_post_election_hook/3
]).

%% @doc Boolean value that indicates if the node is eligible to be a candidate for
%% leader election.
%% Default value is `1true'.
%% @end
candidate_node() ->
    application:get_env(elector, candidate_node, true).

%% @doc Atom indicating if the triggered hook should be executed in the
%% local node or in all nodes.
%% Default value is `local'.
%% @end
hooks_execution() ->
    application:get_env(elector, hooks_execution, local).

%% @doc Returns the configured boolean value that indicates if election
%% should be triggered automatically when node joins or leaves the cluster.
%% Default value is `true'.
%% @end
automatic_elections() ->
    application:get_env(elector, automatic_elections, true).

%% @doc Adds new pre election hook.
%% @end
add_pre_election_hook(Module, Function, Args) ->
    application:set_env(
        elector,
        pre_election_hooks,
        [{Module, Function, Args} | pre_election_hooks()]
    ).

%% @doc Adds new post election hook.
%% @end
add_post_election_hook(Module, Function, Args) ->
    application:set_env(
        elector,
        post_election_hooks,
        [{Module, Function, Args} | post_election_hooks()]
    ).

%% @doc Removes pre election hook.
%% @end
rem_pre_election_hook(Module, Function, Args) ->
    application:set_env(
        elector,
        pre_election_hooks,
        remove_hook(Module, Function, Args, pre_election_hooks())
    ).

%% @doc Removes post election hook.
%% @end
rem_post_election_hook(Module, Function, Args) ->
    application:set_env(
        elector,
        post_election_hooks,
        remove_hook(Module, Function, Args, post_election_hooks())
    ).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
%% @doc Returns the configured election delay.
%% Default value is `1000' (1 second).
%% @end
-spec election_delay() -> Delay :: integer().
election_delay() ->
    application:get_env(elector, election_delay, ?ELECTION_DELAY).

%% @doc Returns the configured strategy module.
%% Default value is `elector_rt_high_strategy' (Node with the highest runtime).
%% @end
-spec strategy_module() -> StrategyModule :: module().
strategy_module() ->
    application:get_env(elector, strategy_module, ?DEFAULT_STRATEGY).

%% @doc Returns the configured pre election hooks.
%% Default value is `[]'.
%% @end
-spec pre_election_hooks() -> Hooks :: [{module(), function(), Args :: list()}].
pre_election_hooks() ->
    application:get_env(elector, pre_election_hooks, []).

%% @doc Returns the configured post election hooks.
%% Default value is `[]'.
%% @end
-spec post_election_hooks() -> Hooks :: [{module(), function(), Args :: list()}].
post_election_hooks() ->
    application:get_env(elector, post_election_hooks, []).

%% @doc Returns the configured quorum size.
%% This is the number of nodes that must be up and running
%% in order to start an election(including the local node).
%% Default value is `1'.
%% @end
-spec quorum_size() -> QuorumSize :: integer().
quorum_size() ->
    application:get_env(elector, quorum_size, 1).

%% @doc Returns boolean value indicating weather the quorum size
%% is met or not.
-spec quorum_check() -> QuorumCheck :: boolean().
quorum_check() ->
    Quorum = elector_config_handler:quorum_size(),
    case Quorum of
        undefined ->
            true;
        _ ->
            Nodes = [node() | nodes()],
            Quorum =< length(Nodes)
    end.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------
%% private
remove_hook(Module, Function, Args, Hooks) ->
    lists:filter(
        fun({M, F, A}) -> M =/= Module orelse F =/= Function orelse A =/= Args end,
        Hooks
    ).
