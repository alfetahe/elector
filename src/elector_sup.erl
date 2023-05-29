%%%-------------------------------------------------------------------
%% @doc elector top level supervisor.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_sup).

%%--------------------------------------------------------------------
%% Behaviours
%%--------------------------------------------------------------------
-behaviour(supervisor).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([start_link/0]).
-export([init/1]).

%%--------------------------------------------------------------------
%% Definitions
%%--------------------------------------------------------------------
-define(SERVER, ?MODULE).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the supervisor process.
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% Callback functions
%%--------------------------------------------------------------------
init([]) ->
    Candidate = #{id => elector_candidate, start => {elector_candidate, start_link, []}},
    ElectionState = #{id => elector_state, start => {elector_state, start_link, []}},
    CommissionOverviewer =
        #{id => elector_overviewer, start => {elector_overviewer, start_link, []}},    

    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [Candidate, ElectionState, CommissionOverviewer],

    {ok, {SupFlags, ChildSpecs}}.
