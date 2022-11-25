-module(elector).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

send_election_msg(Delay) ->
    {ok, Timer_ref} = erlang:send_after(Delay, ?MODULE, election_schedule),
		Timer_ref.

schedule_election(State, Delay) ->
		Delay = if 
			is_integer(Delay) -> Delay;
			true -> application.get_env(elector, election_delay, 5000)
		end.

		if 
			maps:is_key(schedule_election_ref, State) /= true ->
				maps:put(schedule_election_ref, send_election_msg(Delay), State);
		true ->
			State
		end.

init(_) ->
		Sync_start = application:get_env(elector, sync_start, false),
		setup_init(Sync_start).

setup_init(Sync_start) when Sync_start == false ->
    {ok, #{}, {continue, setup}}.
setup_init(Sync_start) when Sync_start == true ->
		{ok, elect(#{})}.

elect(State) ->
		strategy_behaviour:elect(),
		State = maps:remove(schedule_election_ref, State),

handle_continue(setup, State) ->
    net_kernel:monitor_nodes(true),
    schedule_election(State, nil),

    {noreply, State}.

handle_info(election_schedule, State) ->
    {noreply, elect(State)};

handle_info({nodeup, _Node}, State) ->
    {noreply, schedule_election(State, nil};

handle_info({nodedown, _Node}, State) ->
    {noreply, schedule_election(State, nil)};

handle_info(Msg, State) ->
    logger:notice("Unexpected message received at elector: " ++ io:format("~p~n", [Msg])),
    
		{noreply, State}.

handle_call(Msg, _From, State) ->
    {ok, Msg, State}.

handle_cast(_msg, state) ->
    {ok, state}.