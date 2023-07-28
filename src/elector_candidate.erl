%%%-----------------------------------------------------------------------------
%% @doc Candidate process.
%%
%% @private
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_candidate).

%%------------------------------------------------------------------------------
%% Behaviours
%%------------------------------------------------------------------------------
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%------------------------------------------------------------------------------
%% Callback functions
%%------------------------------------------------------------------------------
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
