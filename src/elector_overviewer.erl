-module(elector_overviewer).

% TODO: singleton work

-behaviour(gen_server).

-include("elector_header.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         handle_continue/2]).

start_link(#{name := Name} = Args) ->
    gen_server:start_link({local, ?NAME(Name, "overviewer")},
                          ?MODULE,
                          Args,
                          []).

init(Args) ->
    {ok, Args, {continue, setup}}.

handle_continue(setup, State) ->
    start_manager(State),
    monitor_manager(State),
    {noreply, State}.

handle_call(_, _, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, process, _Object, normal}, State) ->
    {noreply, State};
handle_info({'DOWN', _MonitorRef, process, _Object, _Info}, State) ->
    start_manager(State),
    monitor_manager(State),
    {noreply, State};
handle_info(_, State) ->
    {noreply, State}.

monitor_manager(#{name := Name}) ->
    monitor(process, global:whereis_name(Name)).

start_manager(#{name := Name} = _State) ->
    case elector_state:start_link(Name) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end.
