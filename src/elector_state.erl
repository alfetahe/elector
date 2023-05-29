%%%-------------------------------------------------------------------
%% @doc Main elector state process who's responsibility is to start the
%% election process on start up either syncronously or asyncronously.
%% Also sets up monitoring for node up and down events and triggers
%% automatic election if a node goes down or comes up.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_state).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(gen_server).

-include("elector_header.hrl").

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the elector state process.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%--------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------
init(_) ->
    schedule_checkup(),
    {ok, #{leader_node => undefined}, {continue, set_leader}}.

handle_continue(set_leader, State) ->
    CommissionPid = elector_service:commission_pid(),
    LeaderNode = case CommissionPid of
        undefined ->
            undefined;
        _ ->
            gen_server:call({global, elector_commission}, get_leader)
    end,
    {noreply, maps:put(leader_node, LeaderNode, State)}.

handle_info(leader_checkup, #{leader_node := undefined} = State) ->
    schedule_checkup(),
    gen_server:cast({global, elector_commission}, start_election),
    {noreply, State};  
handle_info(leader_checkup, State) ->
    schedule_checkup(),
    {noreply, State};  
handle_info(_Msg, State) ->
    {noreply, State}.

handle_call({set_leader, LeaderNode}, _From, State) ->
    {reply, ok, maps:put(leader_node, LeaderNode, State)};
handle_call(get_leader, _From, State) ->
    {reply, maps:get(leader_node, State), State};
handle_call(clear_leader, _From, State) ->
    {reply, {ok, leader_cleared}, maps:put(leader_node, undefined, State)};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(clear_leader, State) ->
    {noreply, maps:put(leader_node, undefined, State)};
handle_cast({set_leader, LeaderNode}, State) ->
    {noreply, maps:put(leader_node, LeaderNode, State)};
handle_cast(_msg, state) ->
    {noreply, state}.

%% @private
schedule_checkup() ->
    erlang:send_after(?LEADER_CHECKUP_INTERV, self(), leader_checkup).    