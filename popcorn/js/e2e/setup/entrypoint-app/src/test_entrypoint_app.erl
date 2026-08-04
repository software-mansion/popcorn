-module(test_entrypoint_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Pid = spawn_link(fun idle/0),
    startup_event(os:getenv("POPCORN_STARTUP_EVENT")),
    {ok, Pid}.

stop(_State) ->
    ok.

idle() ->
    receive
        _ -> idle()
    end.

startup_event("send") ->
    ok = wasm:send(#{startup => send});
startup_event("run_js") ->
    wasm:run_js(<<"() => null">>, #{});
startup_event(false) ->
    ok.
