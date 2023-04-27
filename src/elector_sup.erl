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
    ElectionWorker = #{id => elector_worker, start => {elector_worker, start_link, []}},

    % TODO: singleton work
    SingletonOverviewer = #{id => elector_overviewer, start => {elector_overviewer, start_link, [Args]}},

    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ChildSpecs = [ElectionWorker, SingletonOverviewer],

    {ok, {SupFlags, ChildSpecs}}.
