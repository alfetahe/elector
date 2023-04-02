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
%% The election implementation should only contain the logic for
%% selecting the leader node and returning the leader node name.
%% The elect/0 function is triggered on all nodes automatically
%% by the elector application. This means the strategy implementation
%% does not have to worry about starting the election on all nodes.
%% @end
-spec elect() -> Leader :: leader().
elect() ->
    Strategy_module = application:get_env(elector, strategy_module, elector_rt_high_strategy),
    erlang:apply(Strategy_module, elect, []).
