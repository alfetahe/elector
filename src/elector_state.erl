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

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

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
    {ok, #{leader_node => undefined}}.

handle_info(Msg, State) ->
    logger:notice("Unexpected message received at elector state: " ++ io:format("~p", [Msg])),
    {noreply, State}.

handle_call({set_leader, LeaderNode}, _From, State) ->
    {reply, ok, maps:put(leader_node, LeaderNode, State)};
handle_call(get_leader, _From, State) ->
    {reply, maps:get(leader_node, State), State};
handle_call(elect_sync, _From, State) ->
    LeaderNode = gen_server:call({global, elector_singleton}, start_election, 10000),
    {reply, election_finished, maps:put(leader_node, LeaderNode, State)};
handle_call(clear_leader, _From, State) ->
    {reply, {ok, leader_cleared}, maps:put(leader_node, undefined, State)};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(clear_leader, State) ->
    {noreply, maps:put(leader_node, undefined, State)};
handle_cast(_msg, state) ->
    {noreply, state}.
