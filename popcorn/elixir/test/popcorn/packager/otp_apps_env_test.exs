defmodule Popcorn.Packager.OtpAppsEnvTest do
  # ERL_COMPILER_OPTIONS is global
  use ExUnit.Case, async: false

  alias Popcorn.Packager.OtpApps

  @moduletag :tmp_dir

  # It's the developer's, for their own code
  test "ERL_COMPILER_OPTIONS doesn't change the build", %{tmp_dir: tmp_dir} do
    source = Path.join(tmp_dir, "otp_apps_env.erl")

    File.write!(
      source,
      "-module(otp_apps_env).\n-export([ok/0]).\nok() -> ok.\nunused() -> ok.\n"
    )

    {:ok, forms} = :epp.parse_file(to_charlist(source), [])
    module = %{app: :env, module: :otp_apps_env, forms: forms, options: [], chunks: []}
    app = {:application, :env, [modules: [:otp_apps_env]]}
    export = %{otp_version: "#{System.otp_release()}.0", apps: [app], modules: [module]}
    export_dir = Path.join(tmp_dir, "otp-apps")
    File.mkdir_p!(export_dir)

    File.write!(
      Path.join(export_dir, "otp-apps.zst"),
      :zstd.compress(:erlang.term_to_binary(export))
    )

    System.put_env("ERL_COMPILER_OPTIONS", "[warnings_as_errors]")
    on_exit(fn -> System.delete_env("ERL_COMPILER_OPTIONS") end)

    assert {:ok, _build} = OtpApps.compile(export_dir, Path.join(tmp_dir, "cache"))
  end
end
