defmodule Popcorn.Packager.OtpApps do
  @moduledoc false
  # The OTP applications tied to the runtime's emulator, like kernel, whose
  # prim_tty binds NIFs built into it, can't come from the host: Popcorn ships
  # their abstract code, exported from the OTP the runtime is built from by
  # scripts/export-otp-apps.escript. Packaging compiles it with the host's
  # compiler, with the options OTP compiled it with.

  # The applications the export has, if the runtime build has them
  @apps [:kernel, :stdlib, :compiler, :crypto, :asn1]

  def apps, do: @apps

  def export_path(export_dir), do: Path.join(export_dir, "otp-apps.zst")

  def otp_major(version) do
    {major, _rest} = version |> to_string() |> Integer.parse()
    major
  end

  # Compiles the export, once, into a directory in the cache with an
  # <app>/ebin directory for each application. Compile errors are the modules
  # that failed, which the compiler has reported.
  @spec compile(Path.t(), Path.t()) ::
          {:ok, %{dir: Path.t(), otp_version: String.t()}}
          | {:error, :missing | {:unsupported_otp, String.t()} | {:compile_failed, [module()]}}
  def compile(export_dir, cache_dir) do
    with {:ok, export} <- read(export_dir) do
      # Compiled again for another export, for another major OTP, or when this
      # module changes: Mix removes the build of a changed dependency, with the
      # cache, but not of a path one. A build by another release of the same
      # major OTP works, so only the major is in the key.
      hash =
        :sha256
        |> :crypto.hash([System.otp_release(), __MODULE__.module_info(:md5), export])
        |> Base.encode16(case: :lower)

      dir = Path.join(cache_dir, hash)

      if File.dir?(dir) do
        {:ok, %{dir: dir, otp_version: File.read!(Path.join(dir, "OTP_VERSION"))}}
      else
        build(export, dir)
      end
    end
  end

  defp read(export_dir) do
    case File.read(export_path(export_dir)) do
      {:ok, export} -> {:ok, export}
      {:error, _reason} -> {:error, :missing}
    end
  end

  # Built aside and renamed, so that the directory is complete when it exists,
  # even with another packaging building it meanwhile. The host compiles them,
  # so it must be on their major OTP version.
  defp build(export, dir) do
    %{otp_version: version, apps: apps, modules: modules} =
      export |> :zstd.decompress() |> IO.iodata_to_binary() |> :erlang.binary_to_term()

    if otp_major(version) == otp_major(System.otp_release()) do
      tmp_dir = "#{dir}.#{System.unique_integer([:positive])}.tmp"
      File.mkdir_p!(tmp_dir)
      File.write!(Path.join(tmp_dir, "OTP_VERSION"), version)

      case compile_apps(apps, modules, tmp_dir) do
        :ok ->
          install(tmp_dir, dir)
          {:ok, %{dir: dir, otp_version: version}}

        {:error, failed} ->
          File.rm_rf!(tmp_dir)
          {:error, {:compile_failed, failed}}
      end
    else
      {:error, {:unsupported_otp, version}}
    end
  end

  # Builds for other exports are removed
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

  # The forms are preprocessed already. Debug info is for tree-shaking and
  # patching, and deterministic keeps the paths of the headers OTP's build
  # included out of the beams. OTP's Docs and Meta chunks replace the ones the
  # compiler would make from the forms, which have no doc attributes, nor the
  # features preprocessing enabled.
  defp compile_module(out_dir, {app, module, forms, options, docs, chunks}) do
    docs =
      if docs == :none,
        do: [],
        else: [{"Docs", :erlang.term_to_binary(docs, [:deterministic, :compressed])}]

    options =
      [:binary, :report_errors, :deterministic, :debug_info, :no_docs] ++
        [extra_chunks: docs ++ chunks] ++ options

    case :compile.forms(forms, options) do
      {:ok, ^module, beam} ->
        File.write!(Path.join([out_dir, "#{app}", "ebin", "#{module}.beam"]), beam)

      :error ->
        {:error, module}
    end
  end
end
