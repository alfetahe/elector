%%%-------------------------------------------------------------------
%% @doc Module responsible for handling configuration.
%% These settings can be altered with custom settings in the
%% application environment.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_config_handler).

-include("elector_header.hrl").

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([election_delay/0, strategy_module/0, pre_election_hooks/0, post_election_hooks/0,
         startup_hooks_enabled/0, quorum_size/0, quorum_check/0, candidate_node/0, hooks_execution/0]).

candidate_node() ->
    application:get_env(elector, candidate_node, true).

hooks_execution() ->
    application:get_env(elector, hooks_execution, local).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
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

%% @doc Returns the configured startup hooks enabled flag.
%% When turned off the post or pre election hooks will not
%% be executed on the startup.
%% Default value is `true'.
%% @end
-spec startup_hooks_enabled() -> StartupHooksEnabled :: boolean().
startup_hooks_enabled() ->
    application:get_env(elector, startup_hooks_enabled, true).

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
            Quorum =< length([node() | nodes()])
    end.
