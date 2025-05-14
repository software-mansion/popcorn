defmodule Popcorn do
  @moduledoc """
  Library providing Erlang and Elixir stdlibs, allowing
  to compile projects to `.avm` and run them with AtomVM.
  """
  require Popcorn.Config

  @app_build_root Mix.Project.build_path()
  @popcorn_path Mix.Project.app_path()
  @priv_dir :code.priv_dir(:popcorn)
  @api_dir Path.join(["popcorn", "api"])

  @config Popcorn.Config.get([:start_module, :out_dir])

  @doc """
  Packs compiled project files along with the Popcorn Lib into
  a single `.avm` file using the AtomVM's `:packbeam` library.

  If the generated file should be runnable, pass a module implementing
  `start/0` function as the `start_module` option.
  """
  @spec cook([
          {:out_dir, String.t()}
          | {:start_module, module}
          | {:target, :wasm | :unix}
          | {:popcorn_path, String.t()}
          | {:artifacts, [String.t()]}
        ]) :: :ok
  def cook(options \\ []) do
    all_beams = Path.wildcard(Path.join([@app_build_root, "**", "*.{beam,app}"]))
    popcorn_library_path = Path.join(@popcorn_path, "popcorn.avm")

    default_options = [
      out_dir: @config.out_dir,
      start_module: @config.start_module,
      target: :wasm,
      popcorn_path: popcorn_library_path,
      artifacts: all_beams
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()

    File.mkdir_p!(options.out_dir)
    copy_runtime_artifacts(options)

    bundled_artifacts = bundled_artifacts(options.artifacts, options.popcorn_path)

    case pack_bundle(options.out_dir, bundled_artifacts, options.start_module) do
      :ok -> :ok
      {:error, reason} -> raise "Cooking error, reason: #{inspect(reason)}"
    end
  end

  defp bundled_artifacts(all_beams, popcorn_library_path) do
    popcorn_files = Path.wildcard(Path.join([@popcorn_path, "**", "*"]))
    api_beams = Enum.filter(popcorn_files, &popcorn_api_beam?/1)

    # include stdlib bundle, Popcorn.Wasm beam and filter other popcorn beams
    [popcorn_library_path | api_beams] ++ (all_beams -- popcorn_files)
  end

  defp pack_bundle(out_dir, beams, start_module) do
    bundle_path = Path.join(out_dir, "bundle.avm")

    # packbeam appends to the bundle if output exists, we need to delete it first
    maybe_rm(bundle_path)
    bundle_path = to_charlist(bundle_path)
    beams = Enum.map(beams, &String.to_charlist/1)
    :packbeam_api.create(bundle_path, beams, %{start_module: start_module})
  end

  defp maybe_rm(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      error -> raise "Cooking error: couldn't remove old bundle, reason: #{inspect(error)}"
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

  defp copy_runtime_artifacts(options) do
    if options.target == :wasm do
      wasm_template_dir = Path.join([@priv_dir, "static-template", "wasm"])
      File.cp_r!(wasm_template_dir, options.out_dir)
    end

    atomvm_artifacts_dir = Path.join([@popcorn_path, "atomvm_artifacts", "#{options.target}"])
    File.cp_r!(atomvm_artifacts_dir, options.out_dir)
  end
end
