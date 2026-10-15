defmodule Popcorn.PackagerTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureIO

  alias Popcorn.Packager

  @moduletag :tmp_dir

  setup %{tmp_dir: tmp_dir} do
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

    for file <- ["index.mjs", "worker.mjs", "beam.emu.mjs", "beam.wasm"] do
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

    for app <- [:user_app, :extra_app] do
      ebin = Path.join([build_path, to_string(app), "ebin"])
      File.mkdir_p!(ebin)
      props = [vsn: ~c"1.0", modules: [], applications: [:popcorn]]

      File.write!(
        Path.join(ebin, "#{app}.app"),
        :io_lib.format(~c"~tp.~n", [{:application, app, props}])
      )
    end

    {:ok,
     options: [build_path: build_path, out_dir: out_dir, app: "user_app", static_dir: static_dir],
     build_path: build_path,
     out_dir: out_dir}
  end

  describe "build/1" do
    test "complete output", %{options: options, build_path: build_path, out_dir: out_dir} do
      app_dir = Path.join(build_path, "popcorn")
      File.mkdir_p!(Path.join(app_dir, "priv/static"))
      File.write!(Path.join(app_dir, "priv/static/index.mjs"), "Popcorn's JS")
      File.write!(Path.join(app_dir, "priv/nested/native.so"), "native")
      File.mkdir_p!(Path.join(app_dir, "priv/lib/arch"))
      File.write!(Path.join(app_dir, "priv/lib/nif.so"), "native")
      File.write!(Path.join(app_dir, "priv/lib/arch/driver.dylib"), "native")
      File.mkdir_p!(Path.join(app_dir, "priv/plts"))
      File.write!(Path.join(app_dir, "priv/plts/dialyzer.plt"), "PLT")
      File.write!(Path.join(app_dir, "priv/nested/notes.tmp"), "excluded")
      File.mkdir_p!(Path.join(app_dir, "priv/docs"))
      File.write!(Path.join(app_dir, "priv/docs/guide.md"), "excluded")

      options =
        Keyword.merge(options,
          extra_apps: ["extra_app"],
          exclude_priv: [{"popcorn", "docs"}, "nested/*.tmp"]
        )

      assert {:ok, report} = Packager.build(options)
      assert "core" = report.runtimeVariant
      assert %{"popcorn" => _app} = report.apps

      for file <- ["index.mjs", "worker.mjs", "beam.emu.mjs", "beam.wasm"],
          variant <- ["", ".gz", ".br"] do
        assert File.regular?(Path.join(out_dir, file <> variant))
      end

      assert "beam.wasm" = out_dir |> Path.join("beam.wasm.gz") |> File.read!() |> :zlib.gunzip()

      manifest = read_manifest(out_dir)
      assert Map.keys(manifest["bundles"]) |> Enum.sort() == ["app", "deps", "stdlib"]
      assert manifest["apps"]["user_app"]["bundle"] == "app"
      assert manifest["apps"]["extra_app"]["bundle"] == "deps"
      assert manifest["apps"]["popcorn"]["bundle"] == "deps"
      assert manifest["apps"]["elixir"]["bundle"] == "stdlib"
      assert manifest["apps"]["kernel"]["bundle"] == "stdlib"
      assert File.regular?(Path.join(out_dir, "otp/" <> manifest["vm"]["boot"]))
      assert manifest["vm"]["boot"] =~ ~r/^bin\/vm\.[a-f0-9]{64}\.boot$/
      assert length(report.tarPaths) == 3

      for {group, %{"tar" => tar}} <- manifest["bundles"] do
        path = Path.join(out_dir, "otp/" <> tar)
        contents = File.read!(path)
        hash = :crypto.hash(:sha256, contents) |> Base.encode16(case: :lower)
        assert tar == "lib/#{group}.#{hash}.tar"
        assert :zlib.gunzip(File.read!(path <> ".gz")) == contents
        assert {:ok, ^contents} = :brotli.decode(File.read!(path <> ".br"))
      end

      user_archive = read_archive(bundle_path(out_dir, "app"))
      assert Map.has_key?(user_archive, "lib/user_app/ebin/user_app.app")
      refute Map.has_key?(user_archive, "lib/extra_app/ebin/extra_app.app")

      archive = read_archive(bundle_path(out_dir, "deps"))
      assert Map.has_key?(archive, "lib/extra_app/ebin/extra_app.app")
      assert Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.JS.beam")
      assert "from priv" = Map.fetch!(archive, "lib/popcorn/priv/nested/fixture.txt")
      refute Enum.any?(Map.keys(archive), &String.starts_with?(&1, "lib/popcorn/priv/static"))

      # Native libraries and PLT files are left out, and so are the excluded files
      refute Map.has_key?(archive, "lib/popcorn/priv/nested/native.so")
      refute Enum.any?(Map.keys(archive), &String.starts_with?(&1, "lib/popcorn/priv/lib"))
      refute Enum.any?(Map.keys(archive), &String.starts_with?(&1, "lib/popcorn/priv/plts"))
      refute Map.has_key?(archive, "lib/popcorn/priv/nested/notes.tmp")
      refute Enum.any?(Map.keys(archive), &String.starts_with?(&1, "lib/popcorn/priv/docs"))

      refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Packager.beam")
      refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Popcorn.Packager.BeamPatcher.beam")
      refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Treeshake.beam")
      refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Treeshake.Shaker.beam")
      refute Map.has_key?(archive, "lib/popcorn/ebin/treeshake_helper.beam")
      refute Map.has_key?(archive, "lib/popcorn/ebin/Elixir.Mix.Tasks.Popcorn.Cook.beam")

      modules = archive |> Map.fetch!("lib/popcorn/ebin/popcorn.app") |> app_modules()
      assert Popcorn.JS in modules
      refute Enum.any?(modules, &build_tool?/1)

      assert %{"runtimeVariant" => "core", "entrypoint" => "user_app"} = manifest
    end

    test "cache boundaries", %{options: options, build_path: build_path, out_dir: out_dir} do
      app_file = Path.join(build_path, "user_app/ebin/user_app.app")
      dep_file = Path.join(build_path, "popcorn/priv/nested/fixture.txt")

      assert {:ok, _report} = Packager.build(options)
      before = read_manifest(out_dir)
      File.touch!(app_file, 1_000_000)
      File.touch!(dep_file, 2_000_000)
      assert {:ok, _report} = Packager.build(options)
      assert read_manifest(out_dir) == before

      File.write!(app_file, File.read!(app_file) <> "\n")
      assert {:ok, _report} = Packager.build(options)
      after_app = read_manifest(out_dir)
      refute after_app["bundles"]["app"] == before["bundles"]["app"]
      assert after_app["bundles"]["deps"] == before["bundles"]["deps"]
      assert after_app["bundles"]["stdlib"] == before["bundles"]["stdlib"]
      assert after_app["vm"] == before["vm"]
    end

    test "leaving out priv files of applications that aren't packaged", %{options: options} do
      options = Keyword.put(options, :exclude_priv, [{"phoneix", "static"}])

      assert capture_io(:stderr, fn -> assert {:ok, _report} = Packager.build(options) end) =~
               "exclude_priv names applications that aren't packaged: phoneix"
    end
  end

  defp read_manifest(out_dir) do
    out_dir |> Path.join("otp/manifest.json") |> File.read!() |> :json.decode()
  end

  defp bundle_path(out_dir, group) do
    tar = read_manifest(out_dir)["bundles"][group]["tar"]
    Path.join(out_dir, "otp/" <> tar)
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
