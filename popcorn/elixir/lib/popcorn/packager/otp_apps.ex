defmodule Popcorn.Packager.OtpApps do
  @moduledoc false

  @spec compile(Path.t(), Path.t()) ::
          {:ok, %{dir: Path.t(), otp_version: String.t(), apps: [atom()]}}
          | {:error, :missing | {:unsupported_otp, String.t()} | {:compile_failed, [module()]}}
  def compile(export_dir, cache_dir) do
    with {:ok, export} <- read_export(export_dir) do
      hash = export_hash(export)
      dir = Path.join(cache_dir, hash)

      if File.dir?(dir) do
        {:ok, built(dir, File.read!(Path.join(dir, "OTP_VERSION")))}
      else
        build(export, dir)
      end
    end
  end

  defp read_export(export_dir) do
    case File.read(Path.join(export_dir, "otp-apps.zst")) do
      {:ok, export} -> {:ok, export}
      {:error, _reason} -> {:error, :missing}
    end
  end

  defp export_hash(export) do
    :crypto.hash(:sha256, [System.otp_release(), __MODULE__.module_info(:md5), export])
    |> Base.encode16(case: :lower)
  end

  defp build(export, dir) do
    %{otp_version: version, apps: apps, modules: modules} =
      export |> :zstd.decompress() |> IO.iodata_to_binary() |> :erlang.binary_to_term()

    if otp_major(version) == otp_major(System.otp_release()) do
      tmp_dir = "#{dir}.#{System.pid()}-#{System.unique_integer([:positive])}.tmp"
      File.mkdir_p!(tmp_dir)
      File.write!(Path.join(tmp_dir, "OTP_VERSION"), version)

      case compile_apps(apps, modules, tmp_dir) do
        :ok ->
          install(tmp_dir, dir)
          {:ok, built(dir, version)}

        {:error, failed} ->
          File.rm_rf!(tmp_dir)
          {:error, {:compile_failed, failed}}
      end
    else
      {:error, {:unsupported_otp, version}}
    end
  end

  defp built(dir, version) do
    apps =
      for app_file <- Path.wildcard(Path.join(dir, "*/ebin/*.app")),
          do: app_file |> Path.basename(".app") |> String.to_atom()

    %{dir: dir, otp_version: version, apps: Enum.sort(apps)}
  end

  defp install(tmp_dir, dir) do
    with {:error, reason} <- File.rename(tmp_dir, dir) do
      if File.dir?(dir) do
        File.rm_rf!(tmp_dir)
      else
        raise File.RenameError,
          reason: reason,
          action: "rename directory",
          source: tmp_dir,
          destination: dir
      end
    end

    # Remove stale builds
    for other <- dir |> Path.dirname() |> Path.join("*") |> Path.wildcard(),
        other != dir and not String.ends_with?(other, ".tmp") do
      File.rm_rf!(other)
    end
  end

  defp compile_apps(apps, modules, out_dir) do
    for {:application, app, _props} = spec <- apps do
      ebin_dir = Path.join([out_dir, "#{app}", "ebin"])
      File.mkdir_p!(ebin_dir)
      File.write!(Path.join(ebin_dir, "#{app}.app"), :io_lib.format(~c"~tp.~n", [spec]), [:utf8])
    end

    failed =
      modules
      |> Task.async_stream(&compile_module(out_dir, &1), ordered: false, timeout: :infinity)
      |> Enum.flat_map(fn
        {:ok, :ok} -> []
        {:ok, {:error, module}} -> [module]
      end)

    if failed == [], do: :ok, else: {:error, Enum.sort(failed)}
  end

  defp compile_module(out_dir, module) do
    options =
      [:binary, :report_errors, :deterministic, :debug_info, :no_docs] ++
        [extra_chunks: module.chunks] ++ module.options

    case :compile.noenv_forms(module.forms, options) do
      {:ok, name, beam} when name == module.module ->
        File.write!(Path.join([out_dir, "#{module.app}", "ebin", "#{module.module}.beam"]), beam)

      :error ->
        {:error, module.module}
    end
  end

  defp otp_major(version) do
    {major, _rest} = version |> to_string() |> Integer.parse()
    major
  end
end
