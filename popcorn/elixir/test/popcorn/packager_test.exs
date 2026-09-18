defmodule Popcorn.PackagerTest do
  use ExUnit.Case, async: true

  alias Popcorn.Packager

  @tag :tmp_dir
  test "build/1: complete output", %{tmp_dir: tmp_dir} do
    static_dir = Path.join(tmp_dir, "static")
    runtime_dir = Path.join(static_dir, "runtimes/core")
    out_dir = Path.join(tmp_dir, "cooked")
    File.mkdir_p!(runtime_dir)

    for file <- ["worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"] do
      dir = if file == "worker.mjs", do: static_dir, else: runtime_dir
      File.write!(Path.join(dir, file), file)
    end

    manifest = %{
      vm: %{
        version: host_otp_version(),
        preloaded: host_preloaded_modules(),
        capabilities: %{crypto: false}
      }
    }

    File.write!(Path.join(runtime_dir, "manifest.json"), :json.encode(manifest))

    result =
      Packager.build(
        build_path: Path.join(Mix.Project.build_path(), "lib"),
        out_dir: out_dir,
        app: "popcorn",
        static_dir: static_dir
      )

    assert {:ok, report} = result
    assert "core" = report.runtimeVariant
    assert %{"popcorn" => _app} = report.apps

    for file <- ["worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"] do
      assert File.regular?(Path.join(out_dir, file))
    end

    assert File.regular?(Path.join(out_dir, "otp/bin/vm.boot"))
    assert File.regular?(Path.join(out_dir, "otp/lib/popcorn.tar"))
    assert File.regular?(Path.join(out_dir, "otp/lib/popcorn.tar.gz"))

    archive = out_dir |> Path.join("otp/lib/popcorn.tar") |> read_archive()
    assert Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Wasm.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Packager.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Packager.BeamPatcher.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Treeshake.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Treeshake.Shaker.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/treeshake_helper.beam")
    refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Mix.Tasks.Popcorn.Cook.beam")

    modules = archive |> Map.fetch!("lib/popcorn/ebin/popcorn.app") |> app_modules()
    assert Popcorn.Wasm in modules
    refute Enum.any?(modules, &build_tool?/1)

    output_manifest = out_dir |> Path.join("otp/manifest.json") |> File.read!() |> :json.decode()
    assert %{"runtimeVariant" => "core", "entrypoint" => "popcorn"} = output_manifest
  end

  defp read_archive(path) do
    {:ok, entries} = :erl_tar.extract(to_charlist(path), [:memory])
    Map.new(entries, fn {name, contents} -> {to_string(name), contents} end)
  end

  defp app_modules(contents) do
    {:ok, tokens, _line} = contents |> to_charlist() |> :erl_scan.string()
    {:ok, {:application, :popcorn, props}} = :erl_parse.parse_term(tokens)
    Keyword.fetch!(props, :modules)
  end

  defp build_tool?(module) do
    name = Atom.to_string(module)

    String.starts_with?(name <> ".", [
      "Elixir.Popcorn.Packager.",
      "Elixir.Treeshake.",
      "treeshake_helper.",
      "Elixir.Mix.Tasks.Popcorn."
    ])
  end

  defp host_otp_version do
    [to_string(:code.root_dir()), "releases", System.otp_release(), "OTP_VERSION"]
    |> Path.join()
    |> File.read!()
    |> String.trim()
  end

  defp host_preloaded_modules do
    boot_path = Path.join([to_string(:code.root_dir()), "bin", "no_dot_erlang.boot"])
    {:script, _id, commands} = boot_path |> File.read!() |> :erlang.binary_to_term()

    Enum.flat_map(commands, fn
      {:preLoaded, modules} -> Enum.map(modules, &to_string/1)
      _command -> []
    end)
  end
end
