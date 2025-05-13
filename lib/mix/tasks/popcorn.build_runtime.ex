defmodule Mix.Tasks.Popcorn.BuildRuntime do
  @shortdoc "Utility for building AtomVM from source"
  @moduledoc """
  #{@shortdoc}

  The runtime source defaults to FissionVM repo.
  To use different source, configure your project with
  ```
  config :popcorn, runtime_source: <source>
  ```
  where the <source> can be:
  - `{:path, string}` - local path to AtomVM source
  - `{:git, address}` - GIT address to clone the AtomVM from
  - `{:git, address, ref: ref}` - like above, allows specifying
    branch, tag or commit ref

  Then you can run this task with the following options:
  - `target` - `wasm` (default) `unix`
  - `out_dir` - where to output built artifacts (defaults to CWD)
  - `out_name` - used to name the built artifacts (defaults to "AtomVM")
  """

  use Mix.Task
  require Popcorn.Config

  @requirements "deps.compile"

  @build_dir Mix.Project.app_path()
  @priv_dir :code.priv_dir(:popcorn)
  @config Popcorn.Config.get([:runtime_source])
  @options_defaults %{target: "unix", out_dir: ".", out_name: "AtomVM", cmake_opts: ""}

  def run(args) do
    parser_config = [strict: @options_defaults |> Map.keys() |> Keyword.from_keys(:string)]
    {options, _rest} = OptionParser.parse!(args, parser_config)
    options = Map.merge(@options_defaults, Map.new(options))
    {cmake_opts, options} = Map.pop(options, :cmake_opts)
    cmake_opts = cmake_opts |> String.split(" ", trim: true) |> Enum.map(&"-D#{&1}")

    runtime_source =
      case @config.runtime_source do
        {:path, src} -> src
        {:git, uri} -> fetch_repo(uri)
        {:git, uri, opts} -> fetch_repo(uri, opts)
      end

    # Skip building AtomVM stdlib
    File.write!(Path.join(runtime_source, "libs/CMakeLists.txt"), "\n")

    case String.to_existing_atom(options.target) do
      :unix ->
        build_dir = Path.join(runtime_source, "build")
        File.mkdir_p!(build_dir)
        cmd(~w"cmake .. -DAVM_BUILD_RUNTIME_ONLY=1" ++ cmake_opts, cd: build_dir)
        cmd(~w"make -j", cd: build_dir)
        cp_artifact("src/AtomVM", build_dir, options)

      :wasm ->
        build_dir = Path.join(runtime_source, "src/platforms/emscripten/build")
        File.mkdir_p!(build_dir)

        cmd(
          ~w"emcmake cmake .. -DAVM_BUILD_RUNTIME_ONLY=1 -DAVM_EMSCRIPTEN_ENV=web" ++
            cmake_opts,
          cd: build_dir
        )

        cmd(~w"emmake make -j", cd: build_dir)
        wasm_template_dir = Path.join([@priv_dir, "static-template", "wasm"])
        File.mkdir_p!(options.out_dir)
        File.cp_r!(wasm_template_dir, options.out_dir)

        cp_artifact("src/AtomVM.mjs", build_dir, options)
        cp_artifact("src/AtomVM.wasm", build_dir, options)
    end
  end

  defp fetch_repo(addr, opts \\ []) do
    ref = if opts[:ref], do: ["-b", opts[:ref]], else: []
    output = Path.join(@build_dir, "atomvm_src")
    File.rm_rf!(output)
    IO.puts(:stderr, "Cloning AtomVM from #{addr}")
    cmd(["git", "clone", addr] ++ ref ++ ["--", output])
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

  defp cp_artifact(subpath, build_dir, options) do
    File.cp!(
      Path.join(build_dir, subpath),
      Path.join(options.out_dir, options.out_name <> Path.extname(subpath))
    )
  end
end
