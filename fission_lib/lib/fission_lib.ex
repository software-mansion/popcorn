defmodule FissionLib do
  @moduledoc """
  Library providing Erlang and Elixir stdlibs, allowing
  to compile projects to `.avm` and run them with AtomVM.
  """
  alias __MODULE__.Config

  @build_path Mix.Project.build_path()

  @doc """
  Packs compiled project files along with the Fission Lib into
  a single `.avm` file using the AtomVM's `:packbeam` library.

  If the generated file should be runnable, pass a module implementing
  `start/0` function as the `start_module` option.
  """
  @spec pack([
          {:output_path, String.t()}
          | {:start_module, module}
          | {:fission_lib_path, String.t()}
          | {:artifacts, [String.t()]}
        ]) :: :ok
  def pack(options \\ []) do
    options =
      options
      |> Keyword.validate!(
        output_path: Config.get(:out_path),
        start_module: Config.get(:start_module),
        fission_lib_path: "#{@build_path}/fission_lib.avm",
        artifacts: Path.wildcard("#{@build_path}/**/*.{beam,app}")
      )
      |> Map.new()

    :packbeam_api.create(
      ~c"#{options.output_path}",
      Enum.map([options.fission_lib_path] ++ options.artifacts, &String.to_charlist/1),
      %{start_module: options.start_module}
    )
    |> case do
      :ok -> :ok
      {:error, reason} -> raise "Packing error, reason: #{inspect(reason)}"
    end
  end
end
