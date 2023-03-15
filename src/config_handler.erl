%%%-------------------------------------------------------------------
%% @doc Module responsible for handling configuration.
%% These settings can be altered with custom settings in the
%% application environment.
%% @end
%%%-------------------------------------------------------------------
-module(config_handler).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([election_delay/0, strategy_module/0, sync_start/0, pre_election_hooks/0,
         post_election_hooks/0, startup_hooks_enabled/0, quorum_size/0,
         quorum_check/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Returns the configured election delay.
%% Default value is `1000' (1 second).
%% @end
-spec election_delay() -> Delay :: integer().
election_delay() ->
    application:get_env(elector, election_delay, 1000).

%% @doc Returns the configured strategy module.
%% Default value is `runtime_high_strategy' (Node with the highest runtime).
%% @end
-spec strategy_module() -> StrategyModule :: module().
strategy_module() ->
    application:get_env(elector, strategy_module, runtime_high_strategy).

%% @doc Returns the configured sync_start flag.
%% Default value is `true'.
%% @end
-spec sync_start() -> SyncStart :: boolean().
sync_start() ->
    application:get_env(elector, sync_start, true).

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
    Quorum = config_handler:quorum_size(),
    case Quorum of
        undefined ->
            true;
        _ ->
            Quorum =< length(rpc_client:nodes())
    end.

