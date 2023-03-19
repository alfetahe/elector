%%%-------------------------------------------------------------------
%% @doc Defines the base behaviour for election strategies.
%% @end
%%%-------------------------------------------------------------------
-module(elector_strategy_behaviour).

%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------
-callback elect() -> Leader :: leader().

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([elect/0]).

%%--------------------------------------------------------------------
%% Type definitions
%%--------------------------------------------------------------------
-type leader() :: node().

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the election process by triggering the strategy
%% modules elect() function.
%% @end
elect() ->
    Strategy_module = application:get_env(elector, strategy_module, elector_rt_high_strategy),
    erlang:apply(Strategy_module, elect, []).
