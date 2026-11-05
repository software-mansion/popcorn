defmodule Popcorn.Packager.OtpAppsTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  alias Popcorn.Packager.OtpApps

  @moduletag :tmp_dir

  @version "#{System.otp_release()}.0.1"
  @app {:application, :demo, [description: ~c"Demo, café", modules: [:otp_apps_demo]]}
  @meta :erlang.term_to_binary(enabled_features: [:maybe_expr])

  setup %{tmp_dir: tmp_dir} do
    export_dir = Path.join(tmp_dir, "otp-apps")
    forms = forms(tmp_dir, "info() -> {?MODULE, ?LINE}.")
    write_export(export_dir, [module(forms, [{"Meta", @meta}])])
    %{export_dir: export_dir, cache_dir: Path.join(tmp_dir, "cache")}
  end

  test "compiling the export", %{export_dir: export_dir, cache_dir: cache_dir} do
    assert {:ok, %{dir: dir, otp_version: @version, apps: [:demo]}} =
             OtpApps.compile(export_dir, cache_dir)

    assert {:ok, [@app]} = :file.consult(to_charlist(Path.join(dir, "demo/ebin/demo.app")))

    beam_path = Path.join(dir, "demo/ebin/otp_apps_demo.beam")
    beam = File.read!(beam_path)
    {:module, module} = :code.load_binary(:otp_apps_demo, to_charlist(beam_path), beam)
    assert module.info() == {:otp_apps_demo, 3}

    # OTP's Meta chunk, and no docs
    assert {:ok, {_module, [{~c"Meta", @meta}]}} = :beam_lib.chunks(beam, [~c"Meta"])
    assert {:error, :beam_lib, {:missing_chunk, _, ~c"Docs"}} = :beam_lib.chunks(beam, [~c"Docs"])

    # For tree-shaking and patching, with no paths of this machine
    assert {:ok, {_module, [debug_info: {:debug_info_v1, :erl_abstract_code, _code}]}} =
             :beam_lib.chunks(beam, [:debug_info])

    {:ok, {_module, [compile_info: info]}} = :beam_lib.chunks(beam, [:compile_info])
    refute Keyword.has_key?(info, :source)
  end

  test "compiled once for the export",
       %{export_dir: export_dir, cache_dir: cache_dir} = context do
    assert {:ok, %{dir: dir}} = OtpApps.compile(export_dir, cache_dir)
    beam_path = Path.join(dir, "demo/ebin/otp_apps_demo.beam")
    File.touch!(beam_path, 1_000_000)
    assert {:ok, %{dir: ^dir}} = OtpApps.compile(export_dir, cache_dir)
    assert {{1970, 1, 12}, _time} = File.stat!(beam_path).mtime

    # Another export replaces the build
    forms = forms(context.tmp_dir, "info() -> changed.")
    write_export(export_dir, [module(forms)])
    assert {:ok, %{dir: new_dir}} = OtpApps.compile(export_dir, cache_dir)
    refute new_dir == dir
    assert Path.wildcard(Path.join(cache_dir, "*")) == [new_dir]
  end

  test "compile errors", %{export_dir: export_dir, cache_dir: cache_dir} = context do
    forms = forms(context.tmp_dir, "info() -> undefined().")
    write_export(export_dir, [module(forms)])

    assert capture_io(fn ->
             assert {:error, {:compile_failed, [:otp_apps_demo]}} =
                      OtpApps.compile(export_dir, cache_dir)
           end) =~ "function undefined/0 undefined"

    # Compiled again next time
    assert Path.wildcard(Path.join(cache_dir, "*")) == []
  end

  test "without an export", %{tmp_dir: tmp_dir, cache_dir: cache_dir} do
    assert {:error, :missing} = OtpApps.compile(Path.join(tmp_dir, "missing"), cache_dir)
  end

  # The host compiles them. A build in the cache is for the host's major OTP,
  # which is part of its key.
  test "an export of another major OTP",
       %{export_dir: export_dir, cache_dir: cache_dir} = context do
    forms = forms(context.tmp_dir, "info() -> ok.")
    write_export(export_dir, [module(forms)], "1.0")
    assert {:error, {:unsupported_otp, "1.0"}} = OtpApps.compile(export_dir, cache_dir)
    refute File.exists?(cache_dir)
  end

  # The demo module's abstract code, preprocessed like the export's
  defp forms(tmp_dir, function) do
    source = Path.join(tmp_dir, "otp_apps_demo.erl")
    File.write!(source, "-module(otp_apps_demo).\n-export([info/0]).\n#{function}\n")
    {:ok, forms} = :epp.parse_file(to_charlist(source), [])
    forms
  end

  defp module(forms, chunks \\ []) do
    %{app: :demo, module: :otp_apps_demo, forms: forms, options: [], chunks: chunks}
  end

  defp write_export(dir, modules, version \\ @version) do
    export = %{otp_version: version, apps: [@app], modules: modules}
    File.mkdir_p!(dir)
    File.write!(Path.join(dir, "otp-apps.zst"), :zstd.compress(:erlang.term_to_binary(export)))
  end
end
