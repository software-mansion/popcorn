defmodule Mix.Tasks.Popcorn.BuildRuntime do
  @shortdoc "Utility for building AtomVM from source"
  @moduledoc """
  #{@shortdoc}

  It outputs the artifacts in `project_dir/popcorn_runtime_source/artifacts/<target>`.
  To use the built artifacts in Popcorn, configure it with

  ```
  config :popcorn, runtime: {:path, "popcorn_runtime_source/artifacts/<target>", target: target}
  ```

  Options:
    - `git` - source repo URL (defaults to FissionVM repo)
    - `ref` - branch, tag or commit (only works with the `git` option)
    - `path` - path to repo source (can be used instead of `git`)
    - `target` - `wasm` (default) or `unix`
    - `cmake_opts` - string with space-separated `KEY=VALUE` options that
    will be converted to `-DKEY=VALUE` cmake options
  """

  use Mix.Task

  @requirements "app.config"

  @out_dir "popcorn_runtime_source"
  @default_repo "git@github.com:software-mansion-labs/FissionVM.git"

  # credo:disable-for-next-line Credo.Check.Refactor.CyclomaticComplexity
  def run(args) do
    options_defaults = %{path: nil, git: nil, git_ref: nil, cmake_opts: ""}
    parser_config = [strict: [:target | Map.keys(options_defaults)] |> Keyword.from_keys(:string)]
    {options, _rest} = OptionParser.parse!(args, parser_config)
    options = Map.merge(options_defaults, Map.new(options))

    if not Map.has_key?(options, :target) do
      raise "Missing option `target`"
    end

    {source_type, runtime_source} =
      cond do
        options.path && options.git ->
          raise "Both `path` and `git` options were provided, only one can be given at a time"

        options.path ->
          {:path, options.path}

        true ->
          {:git, fetch_repo(options.git || @default_repo, options.git_ref)}
      end

    {cmake_opts, options} = Map.pop(options, :cmake_opts)
    cmake_opts = cmake_opts |> String.split(" ", trim: true) |> Enum.map(&"-D#{&1}")

    artifacts_dir = Path.join([@out_dir, "artifacts", "#{options.target}"])
    File.mkdir_p!(artifacts_dir)
    File.write!(Path.join(@out_dir, ".gitignore"), "*\n")

    # Skip building AtomVM stdlib
    if source_type != :path do
      File.write!(Path.join(runtime_source, "libs/CMakeLists.txt"), "\n")
    end

    case String.to_existing_atom(options.target) do
      :unix ->
        ensure_executables!(~w"cmake make")
        build_dir = Path.join(runtime_source, "build")
        File.mkdir_p!(build_dir)
        cmd(~w"cmake .. -DAVM_BUILD_RUNTIME_ONLY=1" ++ cmake_opts, cd: build_dir)
        cmd(~w"make -j", cd: build_dir)

        cp_artifact("src/AtomVM", build_dir, artifacts_dir)

      :wasm ->
        ensure_executables!(
          ~w"cmake make emcmake",
          "Please make sure you have emscripten instaled."
        )

        build_dir = Path.join(runtime_source, "src/platforms/emscripten/build")
        File.mkdir_p!(build_dir)

        cmd(
          ~w"emcmake cmake .. -DAVM_BUILD_RUNTIME_ONLY=1 -DAVM_EMSCRIPTEN_ENV=web" ++
            cmake_opts,
          cd: build_dir
        )

        cmd(~w"emmake make -j", cd: build_dir)
        cp_artifact("src/AtomVM.mjs", build_dir, artifacts_dir)
        cp_artifact("src/AtomVM.wasm", build_dir, artifacts_dir)

      target ->
        raise "Invalid target #{inspect(target)}, valid targets: unix, wasm"
    end
  end

  defp fetch_repo(addr, ref) do
    output = Path.join(@out_dir, "atomvm")
    File.rm_rf!(output)
    IO.puts(:stderr, "Cloning AtomVM from #{addr}")
    cmd(["git", "clone", addr, "--", output])
    if ref, do: cmd(["git", "checkout", ref])
    output
  end

  defp cmd([command | args], opts \\ []) do
    opts = Keyword.put_new(opts, :use_stdio, false)

    case System.cmd(command, args, opts) do
      {_output, 0} ->
        :ok

      {_output, status} ->
        System.stop(status)
        # Wait because System.stop is async
        Process.sleep(:infinity)
    end

    :ok
  end

  defp cp_artifact(subpath, build_dir, artifacts_dir) do
    File.cp!(Path.join(build_dir, subpath), Path.join(artifacts_dir, Path.basename(subpath)))
  end

  defp ensure_executables!(executables, description \\ "") do
    missing = Enum.reject(executables, &System.find_executable/1)

    if missing != [] do
      raise "Required commands not found: #{inspect(missing)}. #{description}"
    end
  end
end
