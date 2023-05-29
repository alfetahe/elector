-module(elector_service).

-export([exec_election/1, hook_exec/3, commission_pid/0]).

commission_pid() ->
    global:whereis_name(elector_commission).

exec_election(Opts) ->
    ExecuteHooks = maps:get(run_hooks, Opts),
    iterate_hooks(elector_config_handler:pre_election_hooks(), ExecuteHooks),
    LeaderNode = elector_strategy_behaviour:elect(),
    iterate_hooks(elector_config_handler:post_election_hooks(), ExecuteHooks),
    gen_server:call(elector_state, {set_leader, LeaderNode}),
    LeaderNode.

hook_exec({M, F, A}, Caller, Ref) ->
    erlang:apply(M, F, A),
    Caller ! {hook_executed, Ref}.

%% @private
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
    after 3000 ->
        throw({error, election_hook_timed_out})
    end,
    iterate_hooks(Hooks, ExecuteHooks).
