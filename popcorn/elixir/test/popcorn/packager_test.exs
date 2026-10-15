defmodule Popcorn.PackagerTest do
  use ExUnit.Case, async: true

  alias Popcorn.Packager

  @tag :tmp_dir
  test "build/1: complete output", %{tmp_dir: tmp_dir} do
    static_dir = Path.join(tmp_dir, "static")
    runtime_dir = Path.join(static_dir, "runtimes/core")
    build_path = Path.join(tmp_dir, "build/lib")
    app_dir = Path.join(build_path, "popcorn")
    out_dir = Path.join(app_dir, "priv/static/popcorn")
    File.mkdir_p!(runtime_dir)
    File.mkdir_p!(app_dir)

    File.cp_r!(
      Path.join(Mix.Project.build_path(), "lib/popcorn/ebin"),
      Path.join(app_dir, "ebin")
    )

    File.mkdir_p!(Path.join(app_dir, "priv/nested"))
    File.write!(Path.join(app_dir, "priv/nested/fixture.txt"), "from priv")
    File.mkdir_p!(Path.join(app_dir, "priv/static"))
    File.write!(Path.join(app_dir, "priv/static/index.mjs"), "Popcorn's JS")
    File.write!(Path.join(app_dir, "priv/nested/native.so"), "native")
    File.mkdir_p!(Path.join(app_dir, "priv/lib/arch"))
    File.write!(Path.join(app_dir, "priv/lib/nif.so"), "native")
    File.write!(Path.join(app_dir, "priv/lib/arch/driver.dylib"), "native")
    File.write!(Path.join(app_dir, "priv/nested/notes.tmp"), "excluded")
    File.mkdir_p!(Path.join(app_dir, "priv/docs"))
    File.write!(Path.join(app_dir, "priv/docs/guide.md"), "excluded")

    for file <- ["index.mjs", "worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"] do
      dir = if file in ["index.mjs", "worker.mjs"], do: static_dir, else: runtime_dir
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
        build_path: build_path,
        out_dir: out_dir,
        app: "popcorn",
        static_dir: static_dir,
        exclude_priv: ["popcorn/priv/docs", "*/priv/nested/*.tmp"]
      )

    assert {:ok, report} = result
    assert "core" = report.runtimeVariant
    assert %{"popcorn" => _app} = report.apps

    for file <- ["index.mjs", "worker.mjs", "beam.mjs", "beam.emu.mjs", "beam.wasm"],
        variant <- ["", ".gz", ".br"] do
      assert File.regular?(Path.join(out_dir, file <> variant))
    end

    assert "beam.wasm" = out_dir |> Path.join("beam.wasm.gz") |> File.read!() |> :zlib.gunzip()

    assert File.regular?(Path.join(out_dir, "otp/bin/vm.boot"))
    assert File.regular?(Path.join(out_dir, "otp/lib/popcorn.tar"))
    assert File.regular?(Path.join(out_dir, "otp/lib/popcorn.tar.gz"))
    assert File.regular?(Path.join(out_dir, "otp/lib/popcorn.tar.br"))

    archive = out_dir |> Path.join("otp/lib/popcorn.tar") |> read_archive()
    assert Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Wasm.beam")
    assert "from priv" = Map.fetch!(archive, "lib/popcorn/priv/nested/fixture.txt")
    refute Enum.any?(Map.keys(archive), &String.starts_with?(&1, "lib/popcorn/priv/static"))

    # Native libraries are left out, along with directories they leave empty
    {:ok, entries} =
      out_dir |> Path.join("otp/lib/popcorn.tar") |> to_charlist() |> :erl_tar.table()

    entries = Enum.map(entries, &to_string/1)
    refute "lib/popcorn/priv/nested/native.so" in entries
    refute Enum.any?(entries, &String.starts_with?(&1, "lib/popcorn/priv/lib"))

    # So are the excluded ones
    refute "lib/popcorn/priv/nested/notes.tmp" in entries
    refute Enum.any?(entries, &String.starts_with?(&1, "lib/popcorn/priv/docs"))
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

  @tag :tmp_dir
  test "build/1: starting an entrypoint requires Popcorn", %{tmp_dir: tmp_dir} do
    ebin_dir = Path.join(tmp_dir, "build/app/ebin")
    File.mkdir_p!(ebin_dir)
    app = {:application, :app, [vsn: ~c"0.1.0", modules: [], applications: [:kernel, :stdlib]]}
    File.write!(Path.join(ebin_dir, "app.app"), :io_lib.format(~c"~p.~n", [app]))

    assert {:error, %{code: "missing_boot_app", app: "popcorn"}} =
             Packager.build(
               build_path: Path.join(tmp_dir, "build"),
               out_dir: Path.join(tmp_dir, "out"),
               app: "app"
             )
  end

  @tag :tmp_dir
  test "build/1: priv files to leave out must be in priv", %{tmp_dir: tmp_dir} do
    patterns = ["popcorn/priv/static", "popcorn/ebin/*.beam", "popcorn/priv/../ebin"]

    assert {:error,
            %{
              code: "bad_priv_patterns",
              patterns: ["popcorn/ebin/*.beam", "popcorn/priv/../ebin"]
            }} =
             Packager.build(
               build_path: Path.join(tmp_dir, "build"),
               out_dir: Path.join(tmp_dir, "out"),
               exclude_priv: patterns
             )
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
