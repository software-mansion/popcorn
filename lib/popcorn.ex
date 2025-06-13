defmodule Popcorn do
  @moduledoc """
  Popcorn is a tool for running Elixir in the browser.
  """

  @app_build_root Mix.Project.build_path()
  @popcorn_path Mix.Project.app_path()
  @popcorn_bundle_path Path.join(@popcorn_path, "popcorn.avm")
  @priv_dir :code.priv_dir(:popcorn)
  @api_dir Path.join(["popcorn", "api"])
  @mix_env Mix.env()

  defmodule CookingError do
    @moduledoc false
    defexception [:message]
  end

  @doc """
  Generates static artifacts to run the project in the browser.

  `out_dir` and `start_module` are mandatory, unless provided
  via `config.exs`, for example `config :popcorn, out_dir: "static/wasm, start_module: Start"`

  Options:
  - `out_dir` - the directory to write artifacts to
  - `start_module` - a module with `start/0` function, the application's entry point
  - `target` - `wasm` (default) or `unix`. If `unix` is chosed, you need to build the runtime
  first with `mix popcorn.build_runtime --target unix`
  - `compile_artifacts` - compiled BEAMs and other artifacts that should be included
  in the generated bundle. Defaults to all the `.beam` and `.app` files for the application
  and dependencies.
  """
  @spec cook([
          {:out_dir, String.t()}
          | {:start_module, module}
          | {:target, :wasm | :unix}
          | {:compile_artifacts, [String.t()]}
        ]) :: :ok
  def cook(options \\ []) do
    ingredients(options)
    bundle(options)
  end

  def bundle(options \\ []) do
    all_beams = Path.wildcard(Path.join([@app_build_root, "**", "*.{beam,app}"]))

    default_options = [
      out_dir: Popcorn.Config.get(:out_dir),
      start_module: Popcorn.Config.get(:start_module),
      compile_artifacts: all_beams
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    ensure_option_present(options, :out_dir, "Output directory")
    ensure_option_present(options, :start_module, "Start module")

    File.mkdir_p!(options.out_dir)

    bundled_artifacts = bundled_artifacts(options.compile_artifacts)

    case pack_bundle(options.out_dir, bundled_artifacts, options.start_module) do
      :ok -> :ok
      {:error, reason} -> raise CookingError, "Reason: #{inspect(reason)}"
    end
  end

  def ingredients(options) do
    default_options = [
      out_dir: Popcorn.Config.get(:out_dir),
      target: :wasm
    ]

    options = options |> Keyword.validate!(default_options) |> Map.new()
    ensure_option_present(options, :out_dir, "Output directory")

    File.mkdir_p!(options.out_dir)
    copy_runtime_artifacts(options)
    :ok
  end

  defp bundled_artifacts(compile_artifacts) do
    popcorn_files = Path.wildcard(Path.join([@popcorn_path, "**", "*"]))
    api_beams = Enum.filter(popcorn_files, &popcorn_api_beam?/1)

    # include stdlib bundle, Popcorn.Wasm beam and filter other popcorn beams
    [@popcorn_bundle_path | api_beams] ++ (compile_artifacts -- popcorn_files)
  end

  defp pack_bundle(out_dir, beams, start_module) do
    bundle_path = Path.join(out_dir, "bundle.avm")

    # packbeam appends to the bundle if output exists, we need to delete it first
    maybe_rm(bundle_path)
    bundle_path = to_charlist(bundle_path)
    beams = Enum.map(beams, &String.to_charlist/1)

    try do
      :packbeam_api.create(bundle_path, beams, %{start_module: start_module})
    catch
      {:start_module_not_found, _module} ->
        raise CookingError, "Provided start module `#{inspect(start_module)}` has not been found"
    end
  end

  defp maybe_rm(path) do
    case File.rm(path) do
      :ok -> :ok
      {:error, :enoent} -> :ok
      error -> raise CookingError, "Couldn't remove old bundle, reason: #{inspect(error)}"
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

    module =
      if String.starts_with?(module_name, "Elixir.") do
        Module.safe_concat([module_name])
      else
        String.to_existing_atom(module_name)
      end

    src_path = to_string(module.module_info(:compile)[:source])

    String.contains?(src_path, @api_dir)
  end

  defp copy_runtime_artifacts(options) do
    if options.target == :wasm do
      wasm_template_dir = Path.join([@priv_dir, "static-template", "wasm"])
      File.cp_r!(wasm_template_dir, options.out_dir)
    end

    atomvm_artifacts_dir = Path.join([@popcorn_path, "atomvm_artifacts", "#{options.target}"])

    if not File.exists?(atomvm_artifacts_dir) do
      raise CookingError, """
      Couldn't find runtime artifacts for target `#{options.target}`. \
      To build them from source, run \
      `MIX_ENV=#{@mix_env} mix popcorn.build_runtime --target #{options.target}`.
      """
    end

    File.cp_r!(atomvm_artifacts_dir, options.out_dir)
  end

  defp ensure_option_present(options, key, name) do
    if options[key] == nil do
      raise CookingError, """
      #{name} not provided. \
      Please provide the `#{key}` option or configure it by putting \
      `config :popcorn, #{key}: value` in your `config.exs`.
      """
    end
  end
end
