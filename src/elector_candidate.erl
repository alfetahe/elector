-module(elector_candidate).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, #{is_candidate => elector_config_handler:candidate_node()}}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(update_state, _From, State) ->
    {reply, ok, maps:put(is_candidate, elector_config_handler:candidate_node(), State)};
handle_call(is_candidate_node, _From, #{is_candidate := IsCandidate} = State) ->
    {reply, {ok, IsCandidate}, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.
