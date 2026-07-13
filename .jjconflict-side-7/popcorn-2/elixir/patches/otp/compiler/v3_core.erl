-module(v3_core).

-compile({popcorn_patch_private, bin_expand_string/5}).

% Patch reason: for some reason, the compiler converts binaries
% smaller than this to integers (and probably reconstructs them
% later somehow). Since AtomVM doesn't support big ints, it cuts
% off parts of binaries, so we need to limit it to fit in a regular
% integer, and patch functions that rely on it.

%% Matches expansion max segment in v3_kernel.
-define(COLLAPSE_MAX_SIZE_SEGMENT, 64).

bin_expand_string(S, Line, Val, Size, Last) when Size >= ?COLLAPSE_MAX_SIZE_SEGMENT ->
    Combined = popcorn_module:make_combined(Line, Val, Size),
    [Combined|bin_expand_string(S, Line, 0, 0, Last)];
bin_expand_string([H|T], Line, Val, Size, Last) ->
    bin_expand_string(T, Line, (Val bsl 8) bor H, Size+8, Last);
bin_expand_string([], Line, Val, Size, Last) ->
    [popcorn_module:make_combined(Line, Val, Size) | Last].
