-module(elector_candidate).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
    
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.