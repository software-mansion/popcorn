%% Reachable via DemoApp.Application:start/2. Its on_load function is private
%% and never referenced from code, yet the VM runs it on module load.
-module(demo_app_on_load).

-export([ping/0]).

-on_load(init/0).

ping() -> pong.

init() ->
    prepare(),
    'Elixir.DemoApp.OnLoadDep':touch().

prepare() -> ok.
