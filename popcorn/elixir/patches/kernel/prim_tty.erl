%% The emscripten target has no terminal to query, so terminal capability
%% detection is skipped and the state is returned unchanged.
-module(prim_tty).
-compile([{popcorn_patch_private, [{init, 2}]}]).
-export([init/2]).

init(State, {unix, _} = OsType) ->
    case erlang:system_info(system_architecture) of
        "wasm32-unknown-emscripten" -> State;
        _ -> popcorn_module:init(State, OsType)
    end;
init(State, OsType) ->
    popcorn_module:init(State, OsType).
