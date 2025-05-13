defmodule FissionLib do
  @moduledoc """
  Library providing Erlang and Elixir stdlibs, allowing
  to compile projects to `.avm` and run them with AtomVM.
  """
  require FissionLib.Config

  @app_build_root Mix.Project.build_path()
  @fission_path Mix.Project.app_path()

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
    all_beams = Path.wildcard(Path.join([@app_build_root, "**", "*.{beam,app}"]))
    fission_library_path = Path.join(@fission_path, "fission_lib.avm")

    default_options = [
      out_path: @config.out_path,
      start_module: @config.start_module,
      fission_lib_path: fission_library_path,
      artifacts: all_beams
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    artifacts = bundled_artifacts(options.artifacts, options.fission_lib_path)

    case pack_bundle(options.out_path, artifacts, options.start_module) do
      :ok -> :ok
      {:error, reason} -> raise "Packing error, reason: #{inspect(reason)}"
    end
  end

  defp bundled_artifacts(all_beams, fission_library_path) do
    fission_lib_files = Path.wildcard(Path.join([@fission_path, "**", "*"]))

    exported_beam_names =
      "{Elixir.FissionLib.RemoteObject,Elixir.FissionLib.Wasm,Elixir.Jason.Encoder.FissionLib.RemoteObject}.beam"

    api_beams = Path.wildcard(Path.join([@fission_path, "**", exported_beam_names]))
    # include stdlib bundle, Fission.Wasm beam and filter other fission beams
    [fission_library_path | api_beams] ++ (all_beams -- fission_lib_files)
  end

  defp pack_bundle(output_path, beams, start_module) do
    output_directory = Path.dirname(output_path)

    # packbeam appends to the bundle if output exists, we need to delete it first
    with :ok <- maybe_rm(output_path),
         :ok <- File.mkdir_p(output_directory) do
      output_path = to_charlist(output_path)
      beams = Enum.map(beams, &String.to_charlist/1)
      :packbeam_api.create(output_path, beams, %{start_module: start_module})
    end
  end

  defp maybe_rm(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      error -> error
    end
  end
end
