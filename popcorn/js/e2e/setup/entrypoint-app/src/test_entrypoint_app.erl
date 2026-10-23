-module(test_entrypoint_app).
-behaviour(application).
-export([start/2, stop/1, report_no_entrypoint/0]).

start(_Type, _Args) ->
    start(os:getenv("POPCORN_STARTUP_EVENT")).

start("await_ready") ->
    Child = 'Elixir.Task':child_spec(fun() ->
        true = register(await_ready_task, self()),
        ok = 'Elixir.Popcorn.JS':await_ready([]),
        ok = wasm:send(#{async_ready => true}),
        receive
            {wasm, _Payload} ->
                ok = 'Elixir.Popcorn.JS':await_ready([]),
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

%% Run with -run in the package without an entrypoint, where the application is
%% loaded, with its config, but not started
report_no_entrypoint() ->
    Started = lists:keymember(test_entrypoint, 1, application:which_applications()),
    {ok, Answer} = application:get_env(test_entrypoint, answer),
    ok = wasm:send(#{no_entrypoint => #{started => Started, configured_answer => Answer}}).

idle() ->
    receive
        {wasm, Payload} ->
            ok = wasm:send(Payload),
            idle();
        _ -> idle()
    end.

startup_event("bridge") ->
    {ok, 42} = application:get_env(test_entrypoint, answer),
    {ok, warning} = application:get_env(logger, level),
    undefined = application:get_env(popcorn, strip),
    ok = wasm:send(#{configured_answer => 42}),
    ok = wasm:send(#{startup_send => true}),
    42 = wasm:run_js(
        <<"async (_args, {trySend}) => { const result = await trySend('startup_listener', {startup_action: true}); if (!result.ok) throw result.error; return 42; }">>,
        #{}
    ),
    ok = wasm:send(#{startup_run_js => 42});
startup_event("fail") ->
    error(startup_failed);
startup_event("await_ready_timeout") ->
    {error, timeout} = 'Elixir.Popcorn.JS':await_ready([{timeout, 0}]),
    ok = wasm:send(#{await_ready_timeout => true});
startup_event(false) ->
    ok.
