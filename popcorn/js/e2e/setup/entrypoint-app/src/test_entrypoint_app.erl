-module(test_entrypoint_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Pid = spawn_link(fun idle/0),
    true = register(startup_listener, Pid),
    startup_event(os:getenv("POPCORN_STARTUP_EVENT")),
    {ok, Pid}.

stop(_State) ->
    ok.

idle() ->
    receive
        {wasm, Payload} ->
            ok = wasm:send(Payload),
            idle();
        _ -> idle()
    end.

startup_event("bridge") ->
    ok = wasm:send(#{startup_send => true}),
    42 = wasm:run_js(
        <<"async (_args, {send}) => { const result = await send('startup_listener', {startup_action: true}); if (!result.ok) throw result.error; return 42; }">>,
        #{}
    ),
    ok = wasm:send(#{startup_run_js => 42});
startup_event("fail") ->
    error(startup_failed);
startup_event(false) ->
    ok.
