max_args = 20
exports = Enum.map_join(0..max_args, ", ", fn i -> "trace/#{i + 4}" end)

impls =
  Enum.map(0..max_args, fn arity ->
    args = Enum.map_join(1..arity//1, ", ", &"X#{&1}")

    """
    trace(M, F, File, Line#{if args != "", do: ", "}#{args}) ->
        PadCount = get_pad_count(),
        print_trace("call", PadCount + 1, M, F, #{arity}, File, Line),
        put(iex_wasm_call_count, PadCount + 1),
        R = M:F(#{args}),
        put(iex_wasm_call_count, PadCount),
        print_trace("retn", PadCount + 1, M, F, #{arity}, File, Line),
        R.
    """
  end)

module =
  """
  %% File generated automatically with gen_trace.exs
  %% Do not edit
  -module(simple_trace).

  -export([trace_apply/4, #{exports}]).

  trace_apply(F, A, File, Line) ->
    PadCount = get_pad_count(),
    print_trace("appl", PadCount, undefined, F, A, File, Line),
    traced.

  pad(N) -> pad(N, []).
  pad(0, Acc) -> Acc;
  pad(N, Acc) when N > 0 -> pad(N - 1, [$\\s | Acc]).

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
        "\n"
    ),
    traced.

  #{impls}
  """

File.write!("#{__DIR__}/patches/popcorn_lib/utils/simple_trace.erl", module)
