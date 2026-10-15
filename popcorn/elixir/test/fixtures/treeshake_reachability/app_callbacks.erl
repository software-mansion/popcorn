-module(app_callbacks).
-behaviour(application).
-export([start/2, stop/1, prep_stop/1, unused/0]).

start(_Type, _Args) -> {ok, self()}.
stop(_State) -> callbacks_stop_dependency:stop().
prep_stop(State) -> nif_module:used(), declared_nif_module:used(), State.
unused() -> unused.
