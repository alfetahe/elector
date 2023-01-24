-module(config_handler).

-export([election_delay/0, strategy_module/0, sync_start/0, pre_election_hooks/0,
         post_election_hooks/0]).

election_delay() ->
    application:get_env(elector, election_delay, 500).

strategy_module() ->
    application:get_env(elector, strategy_module, runtime_high_strategy).

sync_start() ->
    application:get_env(elector, sync_start, true).

pre_election_hooks() ->
    application:get_env(elector, pre_election_hooks, []).

post_election_hooks() ->
    application:get_env(elector, post_election_hooks, []).
