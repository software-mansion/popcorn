alias FissionLib.AtomVM

for type <- [:elixir, :erlang, :erlang_expr] do
  type
  |> AtomVM.eval_fragment()
  |> AtomVM.compile_quoted([:code])
end

AtomVM.delete_run_artifacts()

ExUnit.start(capture_log: true)
