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
    - `git-ref` - branch, tag or commit (only works with the `git` option)
    - `path` - path to repo source (can be used instead of `git`)
    - `target` - `wasm` (default) or `unix`
    - `cmake-opts` - string with space-separated `KEY=VALUE` options that
    will be converted to `-DKEY=VALUE` cmake options

  This is a thin wrapper around `scripts/build-atomvm.sh`.
  """

  use Mix.Task

  @requirements "app.config"

  @out_dir "popcorn_runtime_source"

  def run(args) do
    options_defaults = %{path: nil, git: nil, git_ref: nil, cmake_opts: nil}
    parser_config = [strict: [:target | Map.keys(options_defaults)] |> Keyword.from_keys(:string)]
    {options, _rest} = OptionParser.parse!(args, parser_config)
    options = Map.merge(options_defaults, Map.new(options))

    if not Map.has_key?(options, :target) do
      raise "Missing option `target`"
    end

    target = options.target

    # Build script args
    script_args = build_script_args(options)

    # Add output directory
    artifacts_dir = Path.join([@out_dir, "artifacts", target])
    File.mkdir_p!(artifacts_dir)
    File.write!(Path.join(@out_dir, ".gitignore"), "*\n")

    script_args = ["--outdir", artifacts_dir | script_args]

    # Add build mode (positional) - always use debug for now
    build_mode = "debug-#{target}"
    script_args = script_args ++ [build_mode]

    # Find script path relative to the popcorn package root
    popcorn_root = Mix.Project.deps_paths()[:popcorn] || File.cwd!()

    script_path =
      Path.join([popcorn_root, "..", "..", "scripts", "build-atomvm.sh"]) |> Path.expand()

    unless File.exists?(script_path) do
      raise "Build script not found at #{script_path}"
    end

    IO.puts(:stderr, "Running: #{script_path} #{Enum.join(script_args, " ")}")

    case System.cmd(script_path, script_args, into: IO.stream(:stdio, :line)) do
      {_output, 0} ->
        :ok

      {_output, status} ->
        System.stop(status)
        # Wait because System.stop is async
        Process.sleep(:infinity)
    end

    :ok
  end

  defp build_script_args(options) do
    args = []

    # Handle source: --path or --git + --git-ref
    args =
      cond do
        options.path && options.git ->
          raise "Both `path` and `git` options were provided, only one can be given at a time"

        options.path ->
          ["--source", options.path | args]

        options.git ->
          source =
            if options.git_ref do
              "#{options.git}##{options.git_ref}"
            else
              raise "When using --git, --git-ref is required"
            end

          ["--source", source | args]

        true ->
          # Use default source from script
          args
      end

    # Handle cmake-opts
    args =
      if options.cmake_opts do
        ["--cmake-opts", options.cmake_opts | args]
      else
        args
      end

    args
  end
end
