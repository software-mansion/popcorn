alias Popcorn.Support.{AtomVM, Browser}

Path.wildcard("tmp/*") |> List.delete("tmp/modules") |> Enum.each(&File.rm_rf!/1)

build_script_path = Path.expand("../../../scripts/build-atomvm.sh", __DIR__)
unix_cmake_opts = "SANITIZER=OFF DEBUG_ASSERTIONS=ON DEBUG_GC=ON"

build_wasm_fixture_assets = fn ->
  {output, status} =
    System.cmd("pnpm", ["run", "build"],
      cd: "test/fixtures/wasm/assets",
      stderr_to_stdout: true
    )

  if status != 0 do
    raise """
    Failed to build wasm test fixture assets.

    #{output}
    """
  end
end

ensure_unix_fixture_runtime = fn ->
  runtime_path = AtomVM.unix_runtime_path()

  if not File.exists?(runtime_path) do
    runtime_dir = Path.dirname(runtime_path)

    {output, status} =
      System.cmd(
        build_script_path,
        ["--outdir", runtime_dir, "--cmake-opts", unix_cmake_opts, "debug-unix"],
        stderr_to_stdout: true
      )

    if status != 0 do
      raise """
      Failed to build unix AtomVM test runtime.

      #{output}
      """
    end
  end
end

target = System.get_env("TARGET", "UNIX") |> String.downcase() |> String.to_atom()
AtomVM.test_target(target)

for type <- [:eval_elixir, :eval_erlang_module, :eval_erlang_expr] do
  type
  |> AtomVM.ast_fragment()
  |> AtomVM.compile_quoted()
end

case target do
  :wasm ->
    build_wasm_fixture_assets.()

    Browser.launch()

  :unix ->
    ensure_unix_fixture_runtime.()
end

ci_opts = if System.get_env("CI") == "true", do: [max_cases: 1], else: []
ExUnit.start([capture_log: true, exclude: [:long_running, skip_target: target]] ++ ci_opts)
