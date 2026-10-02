-module(test_entrypoint_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    start(os:getenv("POPCORN_STARTUP_EVENT")).

start("await_ready") ->
    Child = 'Elixir.Task':child_spec(fun() ->
        true = register(await_ready_task, self()),
        ok = 'Elixir.Popcorn.Wasm':await_ready([]),
        ok = wasm:send(#{async_ready => true}),
        receive
            {wasm, _Payload} ->
                ok = 'Elixir.Popcorn.Wasm':await_ready([]),
                ok = wasm:send(#{after_boot => true})
        end
    end),
    'Elixir.Supervisor':start_link([Child], [{strategy, one_for_one}]);
start(Event) ->
    Pid = spawn_link(fun idle/0),
    true = register(startup_listener, Pid),
    startup_event(Event),
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
startup_event("await_ready_timeout") ->
    {error, timeout} = 'Elixir.Popcorn.Wasm':await_ready([{timeout, 0}]),
    ok = wasm:send(#{await_ready_timeout => true});
startup_event(false) ->
    ok.
