-module(elector).
-behaviour(gen_server).
-export([start/0]).
-export([init/1, handle_call/3, handle_cast/2]).


start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{}}.
        
handle_call(Msg, _From, State) ->
    {ok, Msg, State}.

handle_cast(_msg, state) ->
    {ok, state}.