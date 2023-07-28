%%%-----------------------------------------------------------------------------
%% @doc Module responsible for starting the commission process globally and
%% monitor it.
%% @private
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_overviewer).

-include("elector_header.hrl").

%%------------------------------------------------------------------------------
%% Behaviours
%%------------------------------------------------------------------------------
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Callback functions
%%------------------------------------------------------------------------------
init(_) ->
    {ok, [], {continue, setup}}.

handle_continue(setup, State) ->
    start_manager(),
    monitor_manager(),
    commission_checkup(),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(commission_checkup, State) ->
    case elector_service:commission_pid() of
        undefined ->
            start_manager(),
            monitor_manager();
        _ ->
            ok
    end,
    commission_checkup(),
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, _Object, normal}, State) ->
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, _Object, _Info}, State) ->
    start_manager(),
    monitor_manager(),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% Internal functions
%%------------------------------------------------------------------------------

%% @private
monitor_manager() ->
    monitor(process, elector_service:commission_pid()).

%% @private
start_manager() ->
    case elector_commission:start() of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.

%% @private
commission_checkup() ->
    erlang:send_after(?COMMISSION_CHECKUP_INTERV, self(), commission_checkup).
