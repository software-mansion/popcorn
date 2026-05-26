defmodule Mix.Tasks.Popdoc.Bundle do
  use Mix.Task

  @shortdoc "Builds consumer bundle files for Popdoc docs"

  @excluded_dependency_apps ~w(popcorn popdoc ex_doc)a

  @impl true
  def run(_args) do
    app_name = Mix.Project.config()[:app] || Mix.raise("Mix project app is not configured.")
    Mix.Task.run("compile")

    build_path = Mix.Project.build_path() |> Path.expand()
    build_lib_dir = Path.join(build_path, "lib")
    build_dir = build_path |> Path.dirname() |> Path.join("popdoc_bundle")
    bundle_root = Path.join([build_dir, "output", to_string(app_name)])

    step("Preparing consumer bundle output", fn ->
      File.rm_rf!(bundle_root)
      File.mkdir_p!(bundle_root)
    end)

    step("Building bundle files for #{app_name}", fn ->
      {apps, optional_apps} = runtime_bundle_apps!(app_name, build_lib_dir)

      apps
      |> Enum.filter(&app_in_build?(&1, build_lib_dir))
      |> beam_paths_by_app!(build_lib_dir)
      |> pack_beams!(bundle_root, optional_apps)
    end)
  end

  defp runtime_bundle_apps!(root_app, build_lib_dir) do
    {apps, optional} =
      root_app
      |> app_dependencies!(build_lib_dir)
      |> Enum.reduce(
        {MapSet.new([root_app]), optional_dependencies!(root_app, build_lib_dir)},
        fn app, acc -> collect_dependency_apps(app, build_lib_dir, acc) end
      )

    {apps |> MapSet.to_list() |> Enum.sort(), optional}
  end

  defp collect_dependency_apps(app, _build_lib_dir, acc) when app in @excluded_dependency_apps,
    do: acc

  defp collect_dependency_apps(app, build_lib_dir, {seen, optional} = acc) do
    if MapSet.member?(seen, app) or not app_in_build?(app, build_lib_dir) do
      acc
    else
      optional = MapSet.union(optional, optional_dependencies!(app, build_lib_dir))

      app
      |> app_dependencies!(build_lib_dir)
      |> Enum.reduce({MapSet.put(seen, app), optional}, fn dep_app, nested_acc ->
        collect_dependency_apps(dep_app, build_lib_dir, nested_acc)
      end)
    end
  end

  defp app_dependencies!(app, build_lib_dir) do
    app
    |> app_spec!(build_lib_dir)
    |> then(&(Keyword.get(&1, :applications, []) ++ Keyword.get(&1, :included_applications, [])))
  end

  defp optional_dependencies!(app, build_lib_dir) do
    app
    |> app_spec!(build_lib_dir)
    |> Keyword.get(:optional_applications, [])
    |> MapSet.new()
  end

  defp app_spec!(app, build_lib_dir) do
    app_file = app_file_path(app, build_lib_dir)

    unless File.exists?(app_file) do
      Mix.raise("Popdoc expected compiled application spec for #{inspect(app)} at #{app_file}.")
    end

    case :file.consult(String.to_charlist(app_file)) do
      {:ok, [{:application, ^app, spec}]} ->
        spec

      other ->
        Mix.raise(
          "Popdoc expected #{app_file} to contain one #{inspect(app)} application spec, got: #{inspect(other)}"
        )
    end
  end

  defp app_in_build?(app, build_lib_dir) do
    File.exists?(app_file_path(app, build_lib_dir))
  end

  defp app_file_path(app, build_lib_dir) do
    Path.join([build_lib_dir, to_string(app), "ebin", "#{app}.app"])
  end

  defp beam_paths_by_app!(apps, build_lib_dir) do
    Enum.map(apps, fn app ->
      beam_glob = Path.join([build_lib_dir, to_string(app), "ebin", "*.beam"])
      beam_paths = Path.wildcard(beam_glob)

      if Enum.empty?(beam_paths) do
        Mix.raise("Popdoc expected compiled beams for #{inspect(app)} under #{beam_glob}.")
      end

      {app, beam_paths}
    end)
  end

  defp pack_beams!(beams_by_app, bundle_root, optional_apps) do
    bundle_path = Path.join(bundle_root, "consumer.avm")
    File.mkdir_p!(bundle_root)
    File.rm(bundle_path)

    all_beams = Enum.flat_map(beams_by_app, fn {_app, paths} -> paths end)
    beam_charlists = Enum.map(all_beams, &String.to_charlist/1)

    case :packbeam_api.create(String.to_charlist(bundle_path), beam_charlists) do
      :ok ->
        print_bundle_report(beams_by_app, optional_apps, bundle_path)
        [bundle_path]

      other ->
        Mix.raise(
          "Popdoc failed to build consumer.avm via :packbeam_api.create/2: #{inspect(other)}"
        )
    end
  end

  defp print_bundle_report(beams_by_app, optional_apps, bundle_path) do
    entries_by_app =
      Enum.map(beams_by_app, fn {app, paths} ->
        entries =
          paths
          |> Enum.map(fn p ->
            name = Path.basename(p)
            size = File.stat!(p).size
            parent = name |> String.trim_trailing(".beam") |> drop_last_segment()
            {parent, -size, name, size}
          end)
          |> Enum.sort()
          |> Enum.map(fn {_parent, _neg_size, name, size} -> {name, size} end)

        total = entries |> Enum.map(fn {_n, s} -> s end) |> Enum.sum()
        {app, entries, total}
      end)

    name_width =
      entries_by_app
      |> Enum.flat_map(fn {_app, entries, _t} -> Enum.map(entries, fn {n, _s} -> String.length(n) end) end)
      |> Enum.max(fn -> 0 end)

    Enum.each(entries_by_app, fn {app, entries, total} ->
      optional_tag = if MapSet.member?(optional_apps, app), do: "optional dep, ", else: ""
      Mix.shell().info("Source BEAMs for #{app} (#{optional_tag}#{format_bytes(total)} total):")

      Enum.each(entries, fn {name, size} ->
        Mix.shell().info("  #{String.pad_trailing(name, name_width)} | #{format_bytes(size)}")
      end)
    end)

    Mix.shell().info("\nTotal compressed bundle size: #{format_bytes(File.stat!(bundle_path).size)}\n")
  end

  defp drop_last_segment(name) do
    case String.split(name, ".") do
      [_] -> ""
      parts -> parts |> Enum.drop(-1) |> Enum.join(".")
    end
  end

  defp format_bytes(bytes) when bytes < 1024, do: "#{bytes} B"

  defp format_bytes(bytes) when bytes < 1024 * 1024 do
    "#{Float.round(bytes / 1024, 1)} KiB"
  end

  defp format_bytes(bytes) do
    "#{Float.round(bytes / 1024 / 1024, 1)} MiB"
  end

  defp step(label, fun) do
    Mix.shell().info("==> #{label}...")
    fun.()
  end
end
