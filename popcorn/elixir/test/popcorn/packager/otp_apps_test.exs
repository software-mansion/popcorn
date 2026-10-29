defmodule Popcorn.Packager.OtpAppsTest do
  use ExUnit.Case, async: true

  alias Popcorn.Packager.OtpApps

  @moduletag :tmp_dir

  setup %{tmp_dir: tmp_dir} do
    source_dir = Path.join(tmp_dir, "otp-apps")
    write(source_dir, "OTP_VERSION", "29.0.6\n")

    write(
      source_dir,
      "lib/demo/ebin/demo.app",
      "{application, demo, [{modules, [otp_apps_demo]}]}.\n"
    )

    # The host has kernel/include/file.hrl too
    write(source_dir, "lib/kernel/include/file.hrl", "-define(SHIPPED, shipped).\n")
    write(source_dir, "lib/demo/include/demo.hrl", "-define(INCLUDED, included).\n")

    write(source_dir, "lib/demo/src/otp_apps_demo.erl", """
    -module(otp_apps_demo).
    -export([info/0]).
    -include_lib("kernel/include/file.hrl").
    -include("demo.hrl").
    info() -> {?SHIPPED, ?INCLUDED, ?VSN, ?FILE}.
    """)

    write_modules(source_dir, [
      {:demo, :otp_apps_demo, [:debug_info, {:d, :VSN, ~c"1.2"}, {:i, ~c"lib/demo/include"}]}
    ])

    %{source_dir: source_dir, cache_dir: Path.join(tmp_dir, "cache")}
  end

  test "the OTP version", %{source_dir: source_dir, tmp_dir: tmp_dir} do
    assert {:ok, "29.0.6"} = OtpApps.otp_version(source_dir)
    missing = Path.join(tmp_dir, "missing")
    assert {:error, %{code: "missing_otp_apps", path: ^missing}} = OtpApps.otp_version(missing)
  end

  # Shipped headers, the options the module was compiled with, and ?FILE
  # without the source's directory
  test "compiling as OTP's build does", %{source_dir: source_dir, cache_dir: cache_dir} do
    assert {:ok, build} = OtpApps.compile(source_dir, cache_dir)
    assert File.read!(Path.join(build, "demo/ebin/demo.app")) =~ "otp_apps_demo"

    beam_path = Path.join(build, "demo/ebin/otp_apps_demo.beam")
    beam = File.read!(beam_path)
    {:module, module} = :code.load_binary(:otp_apps_demo, to_charlist(beam_path), beam)
    assert module.info() == {:shipped, :included, ~c"1.2", ~c"otp_apps_demo.erl"}

    # The beam has no paths of this machine
    {:ok, {_module, [compile_info: info]}} = :beam_lib.chunks(beam, [:compile_info])
    refute Keyword.has_key?(info, :source)
  end

  test "builds are cached by their sources", %{source_dir: source_dir, cache_dir: cache_dir} do
    assert {:ok, build} = OtpApps.compile(source_dir, cache_dir)
    beam_path = Path.join(build, "demo/ebin/otp_apps_demo.beam")
    File.touch!(beam_path, 1_000_000)

    # Not compiled again
    assert {:ok, ^build} = OtpApps.compile(source_dir, cache_dir)
    assert {{1970, 1, 12}, _time} = File.stat!(beam_path).mtime

    source = Path.join(source_dir, "lib/demo/src/otp_apps_demo.erl")
    File.write!(source, File.read!(source) <> "% Changed\n")
    assert {:ok, new_build} = OtpApps.compile(source_dir, cache_dir)
    refute new_build == build
    refute File.exists?(build)
  end

  test "compile errors", %{source_dir: source_dir, cache_dir: cache_dir} do
    source = Path.join(source_dir, "lib/demo/src/otp_apps_demo.erl")
    File.write!(source, File.read!(source) <> "broken() -> ?UNDEFINED.\n")

    assert {:error, %{code: "otp_compile_failed", errors: [error]}} =
             OtpApps.compile(source_dir, cache_dir)

    assert error =~ "otp_apps_demo: otp_apps_demo.erl:6:"
    assert error =~ "undefined macro 'UNDEFINED'"
    assert Path.wildcard(Path.join(cache_dir, "*")) == []
  end

  defp write(dir, path, contents) do
    path = Path.join(dir, path)
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, contents)
  end

  defp write_modules(dir, modules) do
    write(dir, "modules.eterm", Enum.map(modules, &:io_lib.format(~c"~tp.~n", [&1])))
  end
end
