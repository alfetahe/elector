-module(elector).
-behaviour(gen_server).
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

schedule_election(Time) ->
    {ok, Timer_ref} = erlang:send_after(Time, ?MODULE, election_schedule),
		Timer_ref.

timer_check(State) ->
		if 
			maps:is_key(schedule_ref, State) /= true ->
				maps:put(schedule_ref, schedule_election(0), State);
		true ->
			State
		end.

init(_) ->
    {ok, #{}, {continue, setup}}.

handle_continue(setup, State) ->
    net_kernel:monitor_nodes(true),
    schedule_election(5000),

    {noreply, State}.

handle_info(election_schedule, State) ->
		strategy_behaviour:elect(),
		State = maps:remove(schedule_election, State),

    {noreply, State};

handle_info({nodeup, _Node}, State) ->
		State = timer_check(State),

    {noreply, State};

handle_info({nodedown, _Node}, State) ->
    State = timer_check(State),

    {noreply, State};

handle_info(Msg, State) ->
    logger:notice("Unexpected message received at elector: " ++ io:format("~p~n", [Msg])),
    
		{noreply, State}.

handle_call(Msg, _From, State) ->
    {ok, Msg, State}.

handle_cast(_msg, state) ->
    {ok, state}.