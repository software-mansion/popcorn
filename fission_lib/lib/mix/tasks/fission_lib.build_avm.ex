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
  - `{:git, address, branch: branch}` - like above, allows specifying
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

  def run(args) do
    unless @config.avm_source do
      raise """
      Please configure AtomVM source with

      config :fission_lib, avm_source: "path/to/atomvm"
      """
    end

    defaults = %{target: :unix, out_dir: ".", out_name: "AtomVM"}

    {options, _rest} =
      OptionParser.parse!(args, strict: defaults |> Map.keys() |> Keyword.from_keys(:string))

    options = Map.merge(defaults, Map.new(options))

    avm_source =
      case @config.avm_source do
        {:path, src} -> src
        {:git, uri} -> fetch_repo(uri)
        {:git, uri, opts} -> fetch_repo(uri, opts)
      end

    pwd = File.cwd!()

    case String.to_existing_atom(options.target) do
      :unix ->
        build_dir = Path.join(avm_source, "build")
        File.mkdir_p!(build_dir)
        File.cd!(build_dir)
        shell("cmake ..")
        shell("make -j #{System.schedulers()}")
        File.cd!(pwd)
        cp_artifact(build_dir, "src/AtomVM", options)

      :wasm ->
        build_dir = Path.join(avm_source, "src/platforms/emscripten/build")
        File.mkdir_p!(build_dir)
        File.cd!(build_dir)
        shell("emcmake cmake .. -DAVM_EMSCRIPTEN_ENV=web")
        shell("emmake make -j #{System.schedulers()}")
        File.cd!(pwd)
        cp_artifact(build_dir, "src/AtomVM.js", options)
        cp_artifact(build_dir, "src/AtomVM.wasm", options)
    end
  end

  defp fetch_repo(addr, opts \\ []) do
    branch = if opts[:branch], do: ["-b", opts[:branch]], else: []
    output = Path.join(@build_dir, "atomvm_src")
    File.rm_rf!(output)
    IO.puts("Cloning AtomVM from #{addr}")

    System.cmd("git", ["clone", addr] ++ branch ++ ["--", output])
    |> handle_shell_status()

    output
  end

  defp shell(cmd) do
    System.shell("#{cmd} 1>&2")
    |> handle_shell_status()
  end

  defp handle_shell_status({_out, 0}), do: :ok

  defp handle_shell_status({_out, status}) do
    if status != 0 do
      System.stop(status)
      Process.sleep(:infinity)
    end

    :ok
  end

  defp cp_artifact(build_dir, subpath, options) do
    File.cp!(
      Path.join(build_dir, subpath),
      Path.join(options.out_dir, "#{options.out_name}#{Path.extname(subpath)}")
    )
  end
end
