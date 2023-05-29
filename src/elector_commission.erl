%%%-------------------------------------------------------------------
%% @doc Main elector state process who's responsibility is to start the
%% election process on start up either syncronously or asyncronously.
%% Also sets up monitoring for node up and down events and triggers
%% automatic election if a node goes down or comes up.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_commission).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0, start/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type election_schedule_ref() :: undefined | reference().
-type state() :: #{schedule_ref => election_schedule_ref(), leader_node => node() | undefined}.

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the elector state process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    case gen_server:start_link({global, ?MODULE}, ?MODULE, #{}, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

start() ->
    case gen_server:start({global, ?MODULE}, ?MODULE, #{}, []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.    


%%--------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------
-spec init(any()) -> {ok, state()}.
init(_) ->
    net_kernel:monitor_nodes(true),
    SheduleRef = schedule_election(undefined, 0),
    {ok, #{schedule_ref => SheduleRef, leader_node => undefined}}.

handle_info(election_schedule, State) ->
    {noreply, start_election(State)};
handle_info({nodeup, _Node}, #{schedule_ref := Sr} = State) ->
    NewState = maps:put(schedule_ref, schedule_election(Sr, undefined), State),
    {noreply, NewState};
handle_info({nodedown, _Node}, #{schedule_ref := Sr} = State) ->
    NewState = maps:put(schedule_ref, schedule_election(Sr, undefined), State),
    {noreply, NewState};
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call(get_leader, _From, State) ->
    {reply, maps:get(leader_node, State), State};    
handle_call(start_election, _From, State) ->
    #{leader_node := LeaderNode} = NewState = start_election(State),
    {reply, LeaderNode, NewState};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(start_election, State) ->
    {noreply, start_election(State)};
handle_cast(_msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% @private
start_election(State) ->
    LeaderNode = elector_service:setup_election(#{run_hooks => true}),
    StateLeader   = maps:put(leader_node, LeaderNode, State),
    maps:put(schedule_ref, maps:put(schedule_ref, undefined, State), StateLeader).

%% @private
schedule_election(ScheduleRef, _Delay) when is_reference(ScheduleRef) ->
    ScheduleRef;
schedule_election(_ScheduleRef, Delay) ->
    QuorumCheck = elector_config_handler:quorum_check(),
    case QuorumCheck of
        true ->
            send_election_msg(Delay);
        false ->
            undefined
    end.

%% @private
send_election_msg(Delay) ->
    AgreedDelay =
        case is_number(Delay) of
            true ->
                Delay;
            false ->
                elector_config_handler:election_delay()
        end,

    erlang:send_after(AgreedDelay, self(), election_schedule).
