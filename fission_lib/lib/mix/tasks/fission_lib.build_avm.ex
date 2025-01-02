defmodule Mix.Tasks.FissionLib.BuildAvm do
  @shortdoc "Utility for building AtomVM from source"
  @moduledoc """
  #{@shortdoc}

  Firstly, configure your project with

  ```
  config :fission_lib, avm_source: <source>
  ```

  where the <source> can be:
  - `{:path, string}` - local path to AtomVM source
  - `{:git, address}` - GIT address to clone the AtomVM from
  - `{:git, address, ref: ref}` - like above, allows specifying
    branch, tag or commit ref

  Then you can run this task with the following options:
  - `target` - `unix` (default) or `wasm`
  - `out_dir` - where to output built artifacts (defaults to CWD)
  - `out_name` - used to name the built artifacts (defaults to "AtomVM")
  """

  use Mix.Task
  require FissionLib.Config

  @requirements "deps.compile"

  @build_dir Mix.Project.app_path()
  @config FissionLib.Config.get([:avm_source])
  @options_defaults %{target: :unix, out_dir: ".", out_name: "AtomVM"}

  def run(args) do
    unless @config.avm_source do
      raise """
      Please configure AtomVM source with

      config :fission_lib, avm_source: "path/to/atomvm"
      """
    end

    parser_config = [strict: @options_defaults |> Map.keys() |> Keyword.from_keys(:string)]
    {options, _rest} = OptionParser.parse!(args, parser_config)
    options = Map.merge(@options_defaults, Map.new(options))

    avm_source =
      case @config.avm_source do
        {:path, src} -> src
        {:git, uri} -> fetch_repo(uri)
        {:git, uri, opts} -> fetch_repo(uri, opts)
      end

    cmd(~w"cmake .", cd: avm_source)

    case String.to_existing_atom(options.target) do
      :unix ->
        build_dir = Path.join(avm_source, "build")
        File.mkdir_p!(build_dir)
        cmd(~w"make -j", cd: build_dir)
        cp_artifact("src/AtomVM", build_dir, options)

      :wasm ->
        build_dir = Path.join(avm_source, "src/platforms/emscripten/build")
        File.mkdir_p!(build_dir)
        cmd(~w"emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web", cd: build_dir)
        cmd(~w"emmake make -j", cd: build_dir)
        cp_artifact("src/AtomVM.js", build_dir, options)
        cp_artifact("src/AtomVM.wasm", build_dir, options)
    end
  end

  defp fetch_repo(addr, opts \\ []) do
    ref = if opts[:ref], do: ["-b", opts[:ref]], else: []
    output = Path.join(@build_dir, "atomvm_src")
    File.rm_rf!(output)
    IO.puts("Cloning AtomVM from #{addr}")
    cmd(["git", "clone", addr] ++ ref ++ ["--", output])
    output
  end

  defp cmd(cmd, opts \\ []) do
    System.cmd(hd(cmd), tl(cmd), [use_stdio: false] ++ opts)
    |> handle_shell_status()
  end

  defp handle_shell_status({_out, 0}), do: :ok

  defp handle_shell_status({_out, status}) do
    System.stop(status)
    Process.sleep(:infinity)
    :ok
  end

  defp cp_artifact(subpath, build_dir, options) do
    File.cp!(
      Path.join(build_dir, subpath),
      Path.join(options.out_dir, options.out_name <> Path.extname(subpath))
    )
  end
end
