-module(test_helper).

-export([test_pre_hook_trigger/1, test_post_hook_trigger/1, trigger_type/1, test_hook/1,
         ping/0]).

trigger_type(Type) ->
    case Type of
        pre ->
            test_pre_hook_trigger;
        post ->
            test_post_hook_trigger
    end.

test_hook(Type) ->
    [{?MODULE, trigger_type(Type), [self()]}].

test_pre_hook_trigger(Pid) ->
    test_hook_trigger(pre, Pid).

test_post_hook_trigger(Pid) ->
    test_hook_trigger(post, Pid).

test_hook_trigger(Type, Pid) ->
    erlang:send(Pid, trigger_type(Type)).

ping() ->
    pong.
