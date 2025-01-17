-module(simple_trace_proc).

-export([trace/1]).

trace({Pid, Reference}) ->
    trace(Pid),
    {Pid, Reference};
trace(Pid) ->
    console:print([pid_to_list(self()), " spawned ", pid_to_list(Pid), "\n"]),
    Tracer = spawn_opt(fun trace_loop/0, []),
    Tracer ! {trace, Pid},
    Pid.

trace_loop() ->
    % register(simple_trace_proc, self()),
    do_trace_loop().

do_trace_loop() ->
    receive
        {trace, Pid} ->
            monitor(process, Pid);
        {'DOWN', _Monitor, process, Pid, Reason} ->
            console:print([
                "Process ",
                pid_to_list(Pid),
                " terminated with reason ",
                io_lib:format("~p", [Reason]),
                "\n"
            ])
    end,
    do_trace_loop().
