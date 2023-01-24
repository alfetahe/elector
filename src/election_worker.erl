-module(election_worker).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_election_msg(Delay) ->
    erlang:send_after(Delay, ?MODULE, election_schedule).

schedule_election(State, Delay) ->
    Delay_val =
        if is_integer(Delay) ->
               Delay;
           true ->
               config_handler:election_delay()
        end,

    ElectionTimerRef = maps:is_key(schedule_election_ref, State),

    if 
        ElectionTimerRef /= true ->
            maps:put(schedule_election_ref, send_election_msg(Delay_val), State);
        true ->
           State
    end.

init(_) ->
    net_kernel:monitor_nodes(true),
    Sync_start = config_handler:sync_start(),
    setup_init(Sync_start).

setup_init(Sync_start) when Sync_start =:= false ->
    {ok, #{}, {continue, setup}};

setup_init(Sync_start) when Sync_start =:= true ->
    {ok, elect(#{})}.

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
    logger:notice("Unexpected message received at elector: " ++ io:format("~p~n", [Msg])),
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

elect(State) ->
    StrategyModule = config_handler:strategy_module(),
    iterate_hooks(config_handler:pre_election_hooks()),
    iterate_hooks(config_handler:post_election_hooks()),
    LeaderNode = erlang:apply(StrategyModule, elect, []),
    maps:put(
        leader_node,
        LeaderNode,
        maps:remove(schedule_election_ref, State)).

iterate_hooks([]) ->
    ok;

iterate_hooks([{Module, Func, Args} | Hooks]) ->
    erlang:apply(Module, Func, Args),
    iterate_hooks(Hooks).