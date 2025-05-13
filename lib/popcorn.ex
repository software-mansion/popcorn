defmodule Popcorn do
  @moduledoc """
  Library providing Erlang and Elixir stdlibs, allowing
  to compile projects to `.avm` and run them with AtomVM.
  """
  require Popcorn.Config

  @app_build_root Mix.Project.build_path()
  @popcorn_path Mix.Project.app_path()
  @api_dir Path.join(["popcorn", "api"])

  @config Popcorn.Config.get([:start_module, :out_path])

  @doc """
  Packs compiled project files along with the Popcorn Lib into
  a single `.avm` file using the AtomVM's `:packbeam` library.

  If the generated file should be runnable, pass a module implementing
  `start/0` function as the `start_module` option.
  """
  @spec pack([
          {:out_path, String.t()}
          | {:start_module, module}
          | {:popcorn_path, String.t()}
          | {:artifacts, [String.t()]}
        ]) :: :ok
  def pack(options \\ []) do
    all_beams = Path.wildcard(Path.join([@app_build_root, "**", "*.{beam,app}"]))
    popcorn_library_path = Path.join(@popcorn_path, "popcorn.avm")

    default_options = [
      out_path: @config.out_path,
      start_module: @config.start_module,
      popcorn_path: popcorn_library_path,
      artifacts: all_beams
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    artifacts = bundled_artifacts(options.artifacts, options.popcorn_path)

    case pack_bundle(options.out_path, artifacts, options.start_module) do
      :ok -> :ok
      {:error, reason} -> raise "Packing error, reason: #{inspect(reason)}"
    end
  end

  defp bundled_artifacts(all_beams, popcorn_library_path) do
    popcorn_files = Path.wildcard(Path.join([@popcorn_path, "**", "*"]))
    api_beams = Enum.filter(popcorn_files, &popcorn_api_beam?/1)

    # include stdlib bundle, Popcorn.Wasm beam and filter other popcorn beams
    [popcorn_library_path | api_beams] ++ (all_beams -- popcorn_files)
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

  defp popcorn_api_beam?(path) do
    in_popcorn_ebin_dir?(path) and beam?(path) and beam_src_in_api_dir?(path)
  end

  defp in_popcorn_ebin_dir?(path) do
    dir = path |> Path.relative_to(@popcorn_path) |> Path.dirname()
    dir == "ebin"
  end

  defp beam?(path), do: Path.extname(path) == ".beam"

  defp beam_src_in_api_dir?(beam_path) do
    module_name = Path.basename(beam_path, ".beam")
    module = Module.safe_concat([module_name])
    src_path = to_string(module.module_info(:compile)[:source])

    String.contains?(src_path, @api_dir)
  end
end
