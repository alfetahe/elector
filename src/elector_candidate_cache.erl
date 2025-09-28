%%%-----------------------------------------------------------------------------
%% @doc Candidate cache manager - maintains a distributed cache of candidate nodes.
%%
%% This module implements a smart caching system that:
%% - Monitors node join/leave events
%% - Maintains local cache of candidate nodes
%% - Broadcasts candidate status on startup and node events
%% - Eliminates need for remote calls during elections
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_candidate_cache).

%%------------------------------------------------------------------------------
%% Behaviours
%%------------------------------------------------------------------------------
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_link/0, get_candidates/0, refresh_local_status/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------
-type candidate_cache() :: #{node() => boolean()}.
-type state() :: #{cache := candidate_cache(), last_refresh := integer()}.

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------

%% @doc Starts the candidate cache manager.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc Returns the current list of candidate nodes from cache.
-spec get_candidates() -> [node()].
get_candidates() ->
    try
        gen_server:call(?MODULE, get_candidates, 1000)
    catch
        _:_ ->
            %% Fallback if cache is not available - return all connected nodes
            [node() | nodes()]
    end.

%% @doc Refreshes local candidate status and broadcasts to cluster.
-spec refresh_local_status() -> ok.
refresh_local_status() ->
    gen_server:cast(?MODULE, refresh_local_status).

%%------------------------------------------------------------------------------
%% Callback functions
%%------------------------------------------------------------------------------

%% @private
-spec init([]) -> {ok, state()}.
init([]) ->
    %% Monitor node events
    net_kernel:monitor_nodes(true, [nodedown_reason]),

    %% Initialize cache with local node
    LocalIsCandidate = check_local_candidate_status(),
    Cache = #{node() => LocalIsCandidate},

    %% Broadcast our status to existing nodes
    broadcast_candidate_status(LocalIsCandidate),

    %% Schedule periodic refresh if configured
    maybe_schedule_refresh(),

    {ok, #{cache => Cache, last_refresh => erlang:system_time(second)}}.

handle_call(get_candidates, _From, #{cache := Cache} = State) ->
    Candidates = [Node || {Node, IsCandidate} <- maps:to_list(Cache), IsCandidate =:= true],
    {reply, Candidates, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(refresh_local_status, State) ->
    NewState = refresh_and_broadcast(State),
    {noreply, NewState};

handle_cast({candidate_status, Node, IsCandidate}, #{cache := Cache} = State) ->
    %% Update cache with remote node's candidate status
    NewCache = Cache#{Node => IsCandidate},
    {noreply, State#{cache => NewCache}};

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({nodeup, Node, _Info}, State) ->
    %% Node joined - refresh our status and send to new node
    LocalIsCandidate = check_local_candidate_status(),
    send_candidate_status(Node, LocalIsCandidate),

    %% Request status from new node
    gen_server:cast({?MODULE, Node}, {request_status, node()}),

    {noreply, State};

handle_info({nodedown, Node, _Reason}, #{cache := Cache} = State) ->
    %% Node left - remove from cache
    NewCache = maps:remove(Node, Cache),
    {noreply, State#{cache => NewCache}};

handle_info({request_status, FromNode}, State) ->
    %% Another node is requesting our candidate status
    LocalIsCandidate = check_local_candidate_status(),
    send_candidate_status(FromNode, LocalIsCandidate),
    {noreply, State};

handle_info(periodic_refresh, State) ->
    %% Periodic refresh (if enabled)
    NewState = refresh_and_broadcast(State),
    maybe_schedule_refresh(),
    {noreply, NewState};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    net_kernel:monitor_nodes(false),
    ok.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
check_local_candidate_status() ->
    try
        case erlang:whereis(elector_candidate) of
            undefined ->
                false;
            Pid when is_pid(Pid) ->
                case erlang:is_process_alive(Pid) of
                    true ->
                        try gen_server:call(Pid, is_candidate_node, 500) of
                            {ok, true} -> true;
                            _ -> false
                        catch
                            _:_ -> false
                        end;
                    false ->
                        false
                end;
            _ ->
                false
        end
    catch
        _:_ -> false
    end.

%% @private
broadcast_candidate_status(IsCandidate) ->
    lists:foreach(
        fun(Node) -> send_candidate_status(Node, IsCandidate) end,
        nodes()
    ).

%% @private
send_candidate_status(Node, IsCandidate) ->
    gen_server:cast({?MODULE, Node}, {candidate_status, node(), IsCandidate}).

%% @private
refresh_and_broadcast(#{cache := Cache} = State) ->
    LocalIsCandidate = check_local_candidate_status(),
    NewCache = Cache#{node() => LocalIsCandidate},
    broadcast_candidate_status(LocalIsCandidate),
    State#{cache => NewCache, last_refresh => erlang:system_time(second)}.

%% @private
maybe_schedule_refresh() ->
    case elector_config_handler:candidate_cache_refresh_interval() of
        0 ->
            %% Periodic refresh disabled
            ok;
        Interval when is_integer(Interval), Interval > 0 ->
            erlang:send_after(Interval * 1000, self(), periodic_refresh)
    end.