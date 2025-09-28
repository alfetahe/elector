%%%-----------------------------------------------------------------------------
%% @doc Module for handling the election process and internal API.
%% @private
%% @end
%%%-----------------------------------------------------------------------------
-module(elector_service).

-include("elector_header.hrl").

%%------------------------------------------------------------------------------
%% Exported API
%%------------------------------------------------------------------------------
-export([setup_election/1, hook_exec/3, commission_pid/0, async_call/2, iterate_hooks/2]).

%%------------------------------------------------------------------------------
%% Exported functions
%%------------------------------------------------------------------------------
commission_pid() ->
    global:whereis_name(elector_commission).

%% @doc Executes a function or MFA tuple on multiple nodes asynchronously.
%% @param Fun - Either a function or MFA tuple {Module, Function, Args}
%% @param Nodes - List of nodes to execute on
%% @end
async_call(Fun, Nodes) ->
    Refs = case Fun of
        {Module, Function, Args} ->
            [{Node, erpc:send_request(Node, Module, Function, Args)} || Node <- Nodes];
        _ when is_function(Fun) ->
            [{Node, erpc:send_request(Node, Fun)} || Node <- Nodes]
    end,
    [{Node, erpc:wait_response(Ref, ?ERPC_TIMEOUT)} || {Node, Ref} <- Refs].

setup_election(Opts) ->
    pre_election(Opts),
    LeaderNode = elector_strategy_behaviour:elect(),
    gen_server:abcast([node() | nodes()], elector_state, {set_leader, LeaderNode}),
    post_election(Opts),
    LeaderNode.

hook_exec({M, F, A}, Caller, Ref) ->
    erlang:apply(M, F, A),
    Caller ! {hook_executed, Ref}.

iterate_hooks(_, false) ->
    ok;
iterate_hooks([], _ExecuteHooks) ->
    ok;
iterate_hooks([Mfa | Hooks], ExecuteHooks) when ExecuteHooks =:= true ->
    Ref = erlang:make_ref(),
    spawn(?MODULE, hook_exec, [Mfa, self(), Ref]),
    receive
        {hook_executed, Ref} ->
            ok
    after ?HOOK_EXEC_TIMEOUT ->
        throw({error, hook_exec_timeout, Mfa})
    end,
    iterate_hooks(Hooks, ExecuteHooks).

%% @private
pre_election(#{run_hooks := ExecuteHooks} = _Opts) ->
    Nodes = election_hook_nodes(),
    Fun = fun() ->
        elector_service:iterate_hooks(
            elector_config_handler:pre_election_hooks(),
            ExecuteHooks
        )
    end,
    async_call(Fun, Nodes).

%% @private
post_election(#{run_hooks := ExecuteHooks} = _Opts) ->
    Nodes = election_hook_nodes(),
    Fun = fun() ->
        elector_service:iterate_hooks(
            elector_config_handler:post_election_hooks(), ExecuteHooks
        )
    end,
    async_call(Fun, Nodes).

%% @private
election_hook_nodes() ->
    case elector_config_handler:hooks_execution() of
        local ->
            [node()];
        global ->
            [node() | nodes()]
    end.


