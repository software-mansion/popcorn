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

    # The packager rewrites some .app files, which must stay readable
    popcorn_app = Path.join(app_dir, "ebin/popcorn.app")
    {:ok, [{:application, :popcorn, props}]} = :file.consult(popcorn_app)
    write_app(popcorn_app, :popcorn, Keyword.put(props, :description, ~c"Popcorn, café"))

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

    File.write!(Path.join(runtime_dir, "manifest.json"), JSON.encode!(manifest))

    for app <- [:user_app, :extra_app] do
      ebin = Path.join([build_path, to_string(app), "ebin"])
      File.mkdir_p!(ebin)

      props = [
        description: ~c"#{app}, café",
        vsn: ~c"1.0",
        modules: [],
        registered: [],
        applications: [:kernel, :stdlib, :elixir, :popcorn]
      ]

      write_app(Path.join(ebin, "#{app}.app"), app, props)
      File.mkdir_p!(Path.join([build_path, to_string(app), "priv"]))
      File.write!(Path.join([build_path, to_string(app), "priv/data.txt"]), "#{app}'s")
    end

    {:ok,
     options: [build_path: build_path, out_dir: out_dir, app: :user_app, static_dir: static_dir],
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
          extra_apps: [:extra_app],
          exclude_priv: [{:popcorn, "docs"}, "nested/*.tmp"]
        )

      assert {:ok, report} = Packager.build(options)
      assert "core" = report.runtimeVariant
      assert %{popcorn: _app} = report.apps

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
      # Popcorn needs sasl only to make the boot script
      refute Map.has_key?(manifest["apps"], "sasl")
      refute File.exists?(Path.join(out_dir, "otp/bin"))

      # The app archive has the boot script, which loads the applications from
      # their specs and turns off distribution, so that booting doesn't need the
      # Erlang parser
      user_archive = read_archive(bundle_path(out_dir, "app"))
      boot = Map.fetch!(user_archive, "bin/vm.boot")
      {:script, _id, commands} = :erlang.binary_to_term(boot)

      assert {:apply, {:application, :load, [{:application, :popcorn, popcorn_spec}]}} =
               Enum.find(
                 commands,
                 &match?({:apply, {:application, :load, [{_, :popcorn, _}]}}, &1)
               )

      # From the staged .app file, not the build's
      assert Popcorn.JS in popcorn_spec[:modules]
      refute Enum.any?(popcorn_spec[:modules], &build_tool?/1)

      # Popcorn.Boot starts the entrypoint, and the archives have no versions
      assert [:kernel, :stdlib] =
               for({:apply, {:application, :start_boot, [app, _type]}} <- commands, do: app)

      for {:path, dirs} <- commands, dir <- dirs do
        assert [_root, "lib", app, "ebin"] = dir |> to_string() |> Path.split()
        refute app =~ "-"
      end

      assert [{:application, :kernel, kernel_spec}] =
               for(
                 {:kernelProcess, :application_controller, {_m, _f, args}} <- commands,
                 do: args
               )
               |> List.flatten()

      assert kernel_spec[:env][:start_distribution] == false
      assert length(report.tarPaths) == 3

      for {group, %{"tar" => tar}} <- manifest["bundles"] do
        path = Path.join(out_dir, "otp/" <> tar)
        contents = File.read!(path)
        hash = :crypto.hash(:sha256, contents) |> Base.encode16(case: :lower)
        assert tar == "lib/#{group}.#{hash}.tar"
        assert :zlib.gunzip(File.read!(path <> ".gz")) == contents
        assert {:ok, ^contents} = :brotli.decode(File.read!(path <> ".br"))
      end

      assert Map.has_key?(user_archive, "lib/user_app/priv/data.txt")
      refute Map.has_key?(user_archive, "lib/extra_app/priv/data.txt")

      archive = read_archive(bundle_path(out_dir, "deps"))
      assert Map.has_key?(archive, "lib/extra_app/priv/data.txt")
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

      # The boot script has the applications' specs
      refute Enum.any?(Map.keys(user_archive) ++ Map.keys(archive), &(Path.extname(&1) == ".app"))

      assert %{"runtimeVariant" => "core", "entrypoint" => "user_app"} = manifest
    end

    test "cache boundaries", %{options: options, build_path: build_path, out_dir: out_dir} do
      app_file = Path.join(build_path, "user_app/priv/data.txt")
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

    test "the treeshake option is true, false or options", %{options: options} do
      for treeshake <- [:yes, [:keep], %{keep: []}] do
        assert_raise ArgumentError, ~r/true, false or a keyword list of options/, fn ->
          Packager.build(Keyword.put(options, :treeshake, treeshake))
        end
      end

      assert_raise ArgumentError, ~r/unknown keys \[:enable\]/, fn ->
        Packager.build(Keyword.put(options, :treeshake, enable: true))
      end

      assert {:ok, %{treeshake: nil}} = Packager.build(Keyword.put(options, :treeshake, false))

      assert {:ok, %{treeshake: %{modules_removed: [_ | _]}}} =
               Packager.build(Keyword.put(options, :treeshake, true))
    end

    test "keeping modules that aren't packaged", %{options: options} do
      keep = [mod: NotPackaged, mfa: {:not_packaged, :run, 0}, mod: Popcorn.Boot]
      options = Keyword.put(options, :treeshake, keep: keep)

      assert capture_io(:stderr, fn -> assert {:ok, _report} = Packager.build(options) end) =~
               "keep names modules that aren't packaged: NotPackaged, :not_packaged\n"
    end

    test "leaving out priv files of applications that aren't packaged", %{options: options} do
      options = Keyword.put(options, :exclude_priv, phoneix: "static")

      assert capture_io(:stderr, fn -> assert {:ok, _report} = Packager.build(options) end) =~
               "exclude_priv names applications that aren't packaged: phoneix"
    end

    test "sasl, when an application needs it", %{options: options, build_path: build_path} do
      app_file = Path.join(build_path, "user_app/ebin/user_app.app")
      {:ok, [{:application, :user_app, props}]} = :file.consult(app_file)
      write_app(app_file, :user_app, Keyword.update!(props, :applications, &[:sasl | &1]))

      assert {:ok, %{apps: %{sasl: _app}}} = Packager.build(options)
    end

    test "config", %{options: options, out_dir: out_dir} do
      config = [
        user_app: [answer: 42],
        logger: [level: :warning],
        popcorn: [strip: true],
        not_packaged: [key: :value]
      ]

      assert {:ok, _report} = Packager.build(Keyword.put(options, :config, config))

      # In the boot script, which loads the applications from their specs
      specs = boot_specs(out_dir)
      assert specs.user_app[:env][:answer] == 42
      assert specs.logger[:env][:level] == :warning
      refute Keyword.has_key?(specs.popcorn[:env], :strip)
      refute Map.has_key?(specs, :not_packaged)
    end

    test "without an entrypoint", %{options: options, out_dir: out_dir} do
      assert {:ok, report} = Packager.build(Keyword.put(options, :app, nil))
      assert %{popcorn: _app} = report.apps
      assert report.entrypoint == nil
      # JSON's null, which the JS reads as no entrypoint
      assert read_manifest(out_dir)["entrypoint"] == nil
    end

    test "booting requires Popcorn", %{tmp_dir: tmp_dir} do
      ebin_dir = Path.join(tmp_dir, "build/app/ebin")
      File.mkdir_p!(ebin_dir)
      app = {:application, :app, [vsn: ~c"0.1.0", modules: [], applications: [:kernel, :stdlib]]}
      File.write!(Path.join(ebin_dir, "app.app"), :io_lib.format(~c"~p.~n", [app]))

      for entrypoint <- [:app, nil] do
        assert {:error, %{code: "missing_boot_app", app: :popcorn}} =
                 Packager.build(
                   build_path: Path.join(tmp_dir, "build"),
                   out_dir: Path.join(tmp_dir, "out"),
                   app: entrypoint
                 )
      end
    end
  end

  # Like Mix writes them, in UTF-8
  defp write_app(path, app, props) do
    File.write!(
      path,
      IO.chardata_to_string(:io_lib.format(~c"~tp.~n", [{:application, app, props}]))
    )
  end

  defp read_manifest(out_dir) do
    out_dir |> Path.join("otp/manifest.json") |> File.read!() |> JSON.decode!()
  end

  defp bundle_path(out_dir, group) do
    tar = read_manifest(out_dir)["bundles"][group]["tar"]
    Path.join(out_dir, "otp/" <> tar)
  end

  defp boot_specs(out_dir) do
    archive = read_archive(bundle_path(out_dir, "app"))
    {:script, _id, commands} = archive |> Map.fetch!("bin/vm.boot") |> :erlang.binary_to_term()

    for {:apply, {:application, :load, [{:application, app, props}]}} <- commands,
        into: %{},
        do: {app, props}
  end

  defp read_archive(path) do
    {:ok, entries} = :erl_tar.extract(to_charlist(path), [:memory])
    Map.new(entries, fn {name, contents} -> {to_string(name), contents} end)
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
