alias Popcorn.Support.{AtomVM, Browser}

Path.wildcard("tmp/*") |> List.delete("tmp/modules") |> Enum.each(&File.rm_rf!/1)

target = System.get_env("TARGET", "UNIX") |> String.downcase() |> String.to_atom()
AtomVM.test_target(target)

for type <- [:eval_elixir, :eval_erlang_module, :eval_erlang_expr] do
  type
  |> AtomVM.ast_fragment()
  |> AtomVM.compile_quoted()
end

case target do
  :unix ->
    Popcorn.ingredients(target: :unix, out_dir: "test/fixtures/unix")

  :wasm ->
    Popcorn.ingredients(target: :wasm, out_dir: "test/fixtures/wasm/static/wasm")
    Browser.launch()
end

ci_opts = if System.get_env("CI") == "true", do: [max_cases: 1], else: []
ExUnit.start([capture_log: true, exclude: [:long_running, :"skip_#{target}"]] ++ ci_opts)
