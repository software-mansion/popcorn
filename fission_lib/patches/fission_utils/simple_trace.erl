%% File generated automatically with gen_trace.exs
%% Do not edit
-module(simple_trace).

-export([trace/4, trace/5, trace/6, trace/7, trace/8, trace/9, trace/10, trace/11, trace/12, trace/13, trace/14, trace/15, trace/16, trace/17, trace/18, trace/19, trace/20, trace/21, trace/22, trace/23, trace/24]).

pad(N) -> pad(N, []).
pad(0, Acc) -> Acc;
pad(N, Acc) when N > 0 -> pad(N - 1, [$\s | Acc]).

trace(M, F, File, Line) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/0 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/1 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/2 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/3 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/4 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/5 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/6 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/7 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/8 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/9 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/10 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/11 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/12 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/13 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/14 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/15 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/16 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/17 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/18 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/19 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20) ->
    Count = case get(iex_wasm_call_count) of
        undefined -> 0;
        N -> N
    end,
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " call ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    put(iex_wasm_call_count, Count + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20),
    put(iex_wasm_call_count, Count),
    console:print([erlang:pid_to_list(self()), pad(Count * 2), " retn ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/20 ", File, ":", erlang:integer_to_list(Line), "\n"]),
    R.

