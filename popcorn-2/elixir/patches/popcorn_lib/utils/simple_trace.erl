%% File generated automatically with gen_trace.exs
%% Do not edit
-module(simple_trace).

-export([trace_apply/4, trace/4, trace/5, trace/6, trace/7, trace/8, trace/9, trace/10, trace/11, trace/12, trace/13, trace/14, trace/15, trace/16, trace/17, trace/18, trace/19, trace/20, trace/21, trace/22, trace/23, trace/24]).

trace_apply(F, A, File, Line) ->
  PadCount = get_pad_count(),
  print_trace("appl", PadCount, undefined, F, A, File, Line),
  traced.

pad(N) -> pad(N, []).
pad(0, Acc) -> Acc;
pad(N, Acc) when N > 0 -> pad(N - 1, [$\s | Acc]).

get_pad_count() ->
  case get(iex_wasm_call_count) of
      undefined -> 0;
      N -> N
  end.

name() ->
  case get('__proc_name__') of
    undefined ->
      Name =
        case process_info(self(), registered_name) of
          [] -> [];
          {registered_name, Name2} -> atom_to_list(Name2)
        end,
      put('__proc_name__', Name),
      Name;
    Name -> Name
  end.

print_trace(Type, PadCount, M, F, A, File, Line) ->
  MStr =
      case M of
          undefined -> [];
          M -> erlang:atom_to_list(M) ++ "."
      end,
  erlang:display_string(stderr,
      erlang:pid_to_list(self()) ++
      " " ++
      name() ++
      " " ++
      pad(PadCount * 2) ++
      " " ++
      Type ++
      " " ++
      MStr ++
      erlang:atom_to_list(F) ++
      "/" ++
      erlang:integer_to_list(A) ++
      " " ++
      File ++
      ":" ++
      erlang:integer_to_list(Line) ++
      "
"
  ),
  traced.

trace(M, F, File, Line) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 0, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 0, File, Line),
    R.
trace(M, F, File, Line, X1) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 1, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 1, File, Line),
    R.
trace(M, F, File, Line, X1, X2) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 2, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 2, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 3, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 3, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 4, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 4, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 5, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 5, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 6, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 6, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 7, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 7, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 8, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 8, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 9, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 9, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 10, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 10, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 11, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 11, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 12, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 12, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 13, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 13, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 14, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 14, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 15, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 15, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 16, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 16, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 17, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 17, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 18, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 18, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 19, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 19, File, Line),
    R.
trace(M, F, File, Line, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20) ->
    PadCount = get_pad_count(),
    print_trace("call", PadCount + 1, M, F, 20, File, Line),
    put(iex_wasm_call_count, PadCount + 1),
    R = M:F(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, X20),
    put(iex_wasm_call_count, PadCount),
    print_trace("retn", PadCount + 1, M, F, 20, File, Line),
    R.

