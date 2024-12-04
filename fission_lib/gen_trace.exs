max_args = 20
exports = Enum.map_join(0..max_args, ", ", fn i -> "trace/#{i + 4}" end)

print = fn action, arity ->
  ~s<console:print([erlang:pid_to_list(self()), pad(Count * 2), " #{action} ", erlang:atom_to_list(M), ".", erlang:atom_to_list(F), "/#{arity} ", File, ":", erlang:integer_to_list(Line), "\\n"])>
end

impls =
  Enum.map(0..max_args, fn arity ->
    args = Enum.map_join(1..arity//1, ", ", &"X#{&1}")

    """
    trace(M, F, File, Line#{if args != "", do: ", "}#{args}) ->
        Count = case get(iex_wasm_call_count) of
            undefined -> 0;
            N -> N
        end,
        #{print.("call", arity)},
        put(iex_wasm_call_count, Count + 1),
        R = M:F(#{args}),
        put(iex_wasm_call_count, Count),
        #{print.("retn", arity)},
        R.
    """
  end)

module =
  """
  %% File generated automatically with gen_trace.exs
  %% Do not edit
  -module(simple_trace).

  -export([#{exports}]).

  pad(N) -> pad(N, []).
  pad(0, Acc) -> Acc;
  pad(N, Acc) when N > 0 -> pad(N - 1, [$\\s | Acc]).

  #{impls}
  """

File.write!("#{__DIR__}/patches/fission_utils/simple_trace.erl", module)
