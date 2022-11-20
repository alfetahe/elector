%%%-------------------------------------------------------------------
%% @doc elector top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(elector_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    Elector = #{id => elector,       
                start => {elector, start_link, []}},
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [Elector],

    {ok, {SupFlags, ChildSpecs}}.
