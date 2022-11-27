-module(config_handler).

-export([get_election_delay/0, get_strategy_module/0, get_sync_start/0]).

get_election_delay() ->
    application:get_env(elector, election_delay, 5000).

get_strategy_module() ->
    application:get_env(elector, strategy_module, runtime_strategy).

get_sync_start() ->
    application:get_env(elector, sync_start, false).