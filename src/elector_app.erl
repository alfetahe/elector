%%%-------------------------------------------------------------------
%% @doc elector application module that bootstraps the elector.
%% @end
%%%-------------------------------------------------------------------
-module(elector_app).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(application).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start/2, stop/1]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the elector application.
-spec start(_StartType :: term(), _StartArgs :: term()) -> ok.
start(_StartType, _StartArgs) ->
    validate(),
    elector_sup:start_link().

-spec stop(_State :: term()) -> ok.
stop(_State) ->
    ok.

%%---------------------------------------------------------------------
%% internal functions
%%---------------------------------------------------------------------
%% @doc Validates the strategy module.
validate() ->
    Strategy_module = config_handler:strategy_module(),

    Attributes = erlang:apply(Strategy_module, module_info, [attributes]),
    Behaviours = proplists:get_value(behaviour, Attributes),
    Is_strategy = lists:member(strategy_behaviour, Behaviours),

    if Is_strategy /= true ->
           throw({strategy_implementation_error,
                  "Strategy module must implement the base strategy behaviour"});
       true ->
           nil
    end,

    ok.
