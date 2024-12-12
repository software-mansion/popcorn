defmodule FissionLib do
  @moduledoc """
  Library providing Erlang and Elixir stdlibs, allowing
  to compile projects to `.avm` and run them with AtomVM.
  """
  require FissionLib.Config

  @build_path Mix.Project.build_path()
  @app_path Mix.Project.app_path()

  @config FissionLib.Config.get([:start_module, :out_path])

  @doc """
  Packs compiled project files along with the Fission Lib into
  a single `.avm` file using the AtomVM's `:packbeam` library.

  If the generated file should be runnable, pass a module implementing
  `start/0` function as the `start_module` option.
  """
  @spec pack([
          {:out_path, String.t()}
          | {:start_module, module}
          | {:fission_lib_path, String.t()}
          | {:artifacts, [String.t()]}
        ]) :: :ok
  def pack(options \\ []) do
    options =
      options
      |> Keyword.validate!(
        out_path: @config.out_path,
        start_module: @config.start_module,
        fission_lib_path: "#{@app_path}/fission_lib.avm",
        artifacts: Path.wildcard("#{@build_path}/**/*.{beam,app}")
      )
      |> Map.new()

    fission_lib_artifacts = Path.wildcard("#{@app_path}/**/*")
    artifacts = [options.fission_lib_path | options.artifacts -- fission_lib_artifacts]

    :packbeam_api.create(~c"#{options.out_path}", Enum.map(artifacts, &String.to_charlist/1), %{
      start_module: options.start_module
    })
    |> case do
      :ok -> :ok
      {:error, reason} -> raise "Packing error, reason: #{inspect(reason)}"
    end
  end
end
