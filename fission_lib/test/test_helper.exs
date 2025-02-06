alias FissionLib.Support.AtomVM

for type <- [:eval_elixir, :eval_erlang_module, :eval_erlang_expr] do
  type
  |> AtomVM.ast_fragment()
  |> AtomVM.compile_quoted()
end

ExUnit.start(capture_log: true)
