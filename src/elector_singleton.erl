-module(elector_singleton).

-behaviour(gen_server).

-export([start_link/1]).
-export([handle_info/2, handle_call/3, handle_cast/2, init/1]).

start_link(_) ->
    case gen_server:start_link({global, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

init(Args) ->
    {ok, Args}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
