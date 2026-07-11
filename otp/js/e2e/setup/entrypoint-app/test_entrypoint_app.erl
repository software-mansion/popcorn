-module(test_entrypoint_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    Pid = spawn_link(fun idle/0),
    ok = wasm:send(#{entrypoint_started => true}),
    {ok, Pid}.

stop(_State) ->
    ok.

idle() ->
    receive
        _ -> idle()
    end.
