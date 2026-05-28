% Patch reason: AtomVM doesn't support timers,
% use atomvm_timer_manager instead

-module(timer).

-export([sleep/1, send_interval/3, cancel/1]).


-spec sleep(Timeout :: timeout()) -> ok.
sleep(Timeout) ->
    receive
    after Timeout ->
        ok
    end.

send_interval(Timeout, Pid, Message) ->
    timer_manager:send_interval(Timeout, Pid, Message).

cancel(TimerRef) ->
    Result = timer_manager:cancel_timer(TimerRef),
    if
        Result -> {ok, cancel};
        true -> {error, badarg}
    end.
