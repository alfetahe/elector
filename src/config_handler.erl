%%%-------------------------------------------------------------------
%% @doc Module responsible for handling configuration.
%% @end
%%%-------------------------------------------------------------------
-module(config_handler).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([election_delay/0, strategy_module/0, sync_start/0, pre_election_hooks/0,
         post_election_hooks/0]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Returns the configured election delay.
-spec election_delay() -> Delay :: integer().
election_delay() ->
    application:get_env(elector, election_delay, 3000).

%% @doc Returns the configured strategy module.
-spec strategy_module() -> StrategyModule :: module().
strategy_module() ->
    application:get_env(elector, strategy_module, runtime_high_strategy).

%% @doc Returns the configured sync_start flag.
-spec sync_start() -> SyncStart :: boolean().
sync_start() ->
    application:get_env(elector, sync_start, true).

%% @doc Returns the configured pre election hooks.
-spec pre_election_hooks() -> Hooks :: [{module(), function(), Args :: list()}].
pre_election_hooks() ->
    application:get_env(elector, pre_election_hooks, []).

%% @doc Returns the configured post election hooks.
-spec post_election_hooks() -> Hooks :: [{module(), function(), Args :: list()}].
post_election_hooks() ->
    application:get_env(elector, post_election_hooks, []).
