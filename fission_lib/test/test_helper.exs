alias FissionLib.Support.AtomVM

# FIXME: Workaround for the `FissionLib.Support.AtomVM` caching not considering changing the patches
File.rm_rf!("tmp")

for type <- [:eval_elixir, :eval_erlang_module, :eval_erlang_expr] do
  type
  |> AtomVM.ast_fragment()
  |> AtomVM.compile_quoted()
end

ExUnit.start(capture_log: true)
