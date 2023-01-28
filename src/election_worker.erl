%%%-------------------------------------------------------------------
%% @doc Main worker process who's responsibility is to start the
%% election process on start up either syncronously or asyncronously.
%% Also sets up monitoring for node up and down events and triggers
%% automatic election if a node goes down or comes up.
%% @end
%%%-------------------------------------------------------------------
-module(election_worker).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the elector worker process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------    
init(_) ->
    net_kernel:monitor_nodes(true),
    SyncStart = config_handler:sync_start(),
    setup_init(SyncStart).

handle_continue(setup, State) ->
    schedule_election(State, nil),
    {noreply, State}.

handle_info(election_schedule, State) ->
    {noreply, elect(State)};
handle_info({nodeup, _Node}, State) ->
    {noreply, schedule_election(State, nil)};
handle_info({nodedown, _Node}, State) ->
    {noreply, schedule_election(State, nil)};
handle_info(Msg, State) ->
    logger:notice("Unexpected message received at elector: " ++ io:format("~p", [Msg])),
    {noreply, State}.

handle_call(get_leader, _From, State) ->
    {reply, maps:get(leader_node, State), State};
handle_call(elect_sync, _From, State) ->
    {reply, election_finished, elect(State)};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(elect_async, State) ->
    {noreply, elect(State)};
handle_cast(_msg, state) ->
    {noreply, state}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------    
%% @private    
elect(State) ->
    StrategyModule = config_handler:strategy_module(),
    ExecuteHooks = config_handler:startup_hooks_enabled(),
    if ExecuteHooks =:= true ->
        iterate_hooks(config_handler:pre_election_hooks()),
        iterate_hooks(config_handler:post_election_hooks())
    end,
    LeaderNode = erlang:apply(StrategyModule, elect, []),
    maps:put(leader_node, LeaderNode, maps:remove(schedule_election_ref, State)).

%% @private      
iterate_hooks([]) ->
    ok;
iterate_hooks([{Module, Func, Args} | Hooks]) ->
    spawn(erlang, apply, [Module, Func, Args]),
    iterate_hooks(Hooks).

%% @private      
send_election_msg(Delay) ->
    erlang:send_after(Delay, ?MODULE, election_schedule).

%% @private      
schedule_election(State, Delay) ->
    Delay_val =
        if is_integer(Delay) ->
                Delay;
            true ->
                config_handler:election_delay()
        end,

    ElectionTimerRef = maps:is_key(schedule_election_ref, State),

    if ElectionTimerRef /= true ->
            maps:put(schedule_election_ref, send_election_msg(Delay_val), State);
        true ->
            State
    end.

%% @private  
setup_init(Sync_start) when Sync_start =:= false ->
    {ok, #{}, {continue, setup}};
setup_init(Sync_start) when Sync_start =:= true ->
    {ok, elect(#{})}.    
