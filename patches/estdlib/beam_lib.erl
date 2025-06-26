-module(beam_lib).
-export([chunks/2]).

%% Patch reason: Parallel Checker uses this function to chunk module binaries
%% as it still does not work in AtomVM.
chunks(_File, _Chunks) ->
  {'error', 'beam_lib', {'unknown_chunk', "", undefined}}.
