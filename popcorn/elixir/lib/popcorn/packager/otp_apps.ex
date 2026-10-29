defmodule Popcorn.Packager.OtpApps do
  @moduledoc false
  # The OTP applications tied to the runtime's emulator, like kernel, whose
  # prim_tty binds NIFs built into it, can't come from the host: Popcorn ships
  # their sources, exported from the OTP the runtime is built from by
  # scripts/export-otp-apps.escript. They're compiled with the host's compiler,
  # with the options OTP compiled them with, into a cache.

  # The applications the export has, if the runtime build has them
  @apps [:kernel, :stdlib, :compiler, :crypto, :asn1]

  def apps, do: @apps

  @spec otp_version(Path.t()) :: {:ok, String.t()} | {:error, map()}
  def otp_version(source_dir) do
    case File.read(Path.join(source_dir, "OTP_VERSION")) do
      {:ok, version} -> {:ok, String.trim(version)}
      {:error, _reason} -> {:error, %{code: "missing_otp_apps", path: source_dir}}
    end
  end

  # Returns a directory with an <app>/ebin directory for each application
  @spec compile(Path.t(), Path.t()) :: {:ok, Path.t()} | {:error, map()}
  def compile(source_dir, cache_dir) do
    case :file.consult(to_charlist(Path.join(source_dir, "modules.eterm"))) do
      {:ok, modules} ->
        target = Path.join(cache_dir, cache_key(source_dir))
        if File.dir?(target), do: {:ok, target}, else: build(source_dir, modules, target)

      {:error, _reason} ->
        {:error, %{code: "missing_otp_apps", path: source_dir}}
    end
  end

  # Different sources or another compiler make different code
  defp cache_key(source_dir) do
    Application.load(:compiler)

    files =
      source_dir
      |> Path.join("**")
      |> Path.wildcard(match_dot: true)
      |> Enum.filter(&File.regular?/1)
      |> Enum.sort()

    compiler = [System.otp_release(), Application.spec(:compiler, :vsn)]

    files
    |> Enum.reduce(:crypto.hash_update(:crypto.hash_init(:sha256), compiler), fn file, hash ->
      hash
      |> :crypto.hash_update(Path.relative_to(file, source_dir))
      |> :crypto.hash_update(File.read!(file))
    end)
    |> :crypto.hash_final()
    |> Base.encode16(case: :lower)
    |> binary_part(0, 16)
  end

  defp build(source_dir, modules, target) do
    tmp_dir = "#{target}.#{System.unique_integer([:positive])}.tmp"

    for app_file <- Path.wildcard(Path.join(source_dir, "lib/*/ebin/*.app")) do
      ebin_dir = Path.join([tmp_dir, Path.basename(app_file, ".app"), "ebin"])
      File.mkdir_p!(ebin_dir)
      File.cp!(app_file, Path.join(ebin_dir, Path.basename(app_file)))
    end

    errors =
      modules
      |> Task.async_stream(&compile_module(source_dir, tmp_dir, &1),
        ordered: false,
        timeout: :infinity
      )
      |> Enum.flat_map(fn
        {:ok, :ok} -> []
        {:ok, {:error, error}} -> [error]
      end)

    if errors == [] do
      install(tmp_dir, target)
      {:ok, target}
    else
      File.rm_rf!(tmp_dir)
      {:error, %{code: "otp_compile_failed", errors: Enum.sort(errors)}}
    end
  end

  # Like OTP's build: ?FILE is the source's name, as OTP compiles them from
  # their directory, which deterministic also does for the paths in the beams.
  # Include directories are the exported ones, which -include_lib searches
  # before the host's applications.
  defp compile_module(source_dir, out_dir, {app, module, options}) do
    source = Path.join([source_dir, "lib", "#{app}", "src", "#{module}.erl"])

    {includes, options} =
      Enum.split_with(options, &match?({:i, _dir}, &1))

    includes =
      for {:i, dir} <- includes ++ [{:i, "lib"}],
          do: {:i, to_charlist(Path.join(source_dir, dir))}

    case :compile.file(
           to_charlist(source),
           [:binary, :return_errors, :deterministic] ++ includes ++ options
         ) do
      {:ok, ^module, beam} ->
        File.write!(Path.join([out_dir, "#{app}", "ebin", "#{module}.beam"]), beam)

      {:error, errors, _warnings} ->
        {:error, "#{module}: #{format_errors(errors)}"}
    end
  end

  defp format_errors(errors) do
    Enum.map_join(errors, "; ", fn {file, file_errors} ->
      Enum.map_join(file_errors, "; ", fn {location, mod, description} ->
        "#{Path.basename(to_string(file))}:#{format_location(location)}: " <>
          to_string(mod.format_error(description))
      end)
    end)
  end

  defp format_location({line, column}), do: "#{line}:#{column}"
  defp format_location(line), do: "#{line}"

  # Another packaging may have compiled them meanwhile. Other sources' builds
  # are removed.
  defp install(tmp_dir, target) do
    with {:error, reason} <- File.rename(tmp_dir, target) do
      if File.dir?(target) do
        File.rm_rf!(tmp_dir)
      else
        raise File.RenameError,
          reason: reason,
          action: "rename directory",
          source: tmp_dir,
          destination: target
      end
    end

    for dir <- target |> Path.dirname() |> Path.join("*") |> Path.wildcard(),
        dir != target and not String.ends_with?(dir, ".tmp") do
      File.rm_rf!(dir)
    end
  end
end
