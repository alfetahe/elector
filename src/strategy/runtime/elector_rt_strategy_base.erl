%%%-------------------------------------------------------------------
%% @doc Runtime strategy base functionality module used by the
%% high and low runtime strategies.
%% @private
%% @end
%%%-------------------------------------------------------------------
-module(elector_rt_strategy_base).

%%--------------------------------------------------------------------
%% Exported API
%%--------------------------------------------------------------------
-export([elect/1]).

%%--------------------------------------------------------------------
%% Exported functions
%%--------------------------------------------------------------------
%% @doc Starts the election process.
-spec elect(Type :: high | low) -> Leader :: elector_strategy_behaviour:leader().
elect(Type) ->
    CandidateRefs =
        [{Node, erpc:send_request(Node, fun() -> election_data() end)}
         || Node <- [node() | nodes()]],
    CandiateResps = [{Node, erpc:receive_response(Ref)} || {Node, Ref} <- CandidateRefs],
    selected_leader(CandiateResps, Type).

election_data() ->
    {Runtime, _} = erlang:statistics(runtime),
    Runtime.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
%% @private
selected_leader(Runtimes, Type) ->
    CompareFn =
        fun (NodeRuntime, {AccNode, _}) when AccNode =:= undefined ->
                NodeRuntime;
            ({Node, Runtime}, {_, AccRuntime}) when Type =:= high andalso Runtime > AccRuntime ->
                {Node, Runtime};
            ({Node, Runtime}, {_, AccRuntime}) when Type =:= low andalso Runtime < AccRuntime ->
                {Node, Runtime};
            (_, Acc) ->
                Acc
        end,

    {Node, _Runtime} = lists:foldl(CompareFn, {undefined, 0}, Runtimes),
    Node.
