%%%-------------------------------------------------------------------
%% @doc Main elector state process who's responsibility is to start the
%% election process on start up either syncronously or asyncronously.
%% Also sets up monitoring for node up and down events and triggers
%% automatic election if a node goes down or comes up.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_singleton).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(gen_server).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type schedule_ref() :: undefined | reference().

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

%%--------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------
-spec init(any()) -> {ok, schedule_ref()}.
init(_) ->
    net_kernel:monitor_nodes(true),
    SheduleRef = schedule_election(undefined, 0),
    {ok, SheduleRef}.

handle_info(election_schedule, _ScheduleRef) ->
    elector_service:exec_election(#{run_hooks => true}),
    {noreply, undefined};
handle_info({nodeup, _Node}, ScheduleRef) ->
    {noreply, schedule_election(ScheduleRef, undefined)};
handle_info({nodedown, _Node}, ScheduleRef) ->
    {noreply, schedule_election(ScheduleRef, undefined)};
handle_info(Msg, ScheduleRef) ->
    logger:notice("Unexpected message received at elector singleton: "
                  ++ io:format("~p", [Msg])),
    {noreply, ScheduleRef}.

handle_call(start_election, _From, _ScheduleRef) ->
    LeaderNode = elector_service:exec_election(#{run_hooks => true}),
    {reply, LeaderNode, undefined};
handle_call(Msg, _From, ScheduleRef) ->
    {reply, Msg, ScheduleRef}.

handle_cast(_msg, ScheduleRef) ->
    {noreply, ScheduleRef}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

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
