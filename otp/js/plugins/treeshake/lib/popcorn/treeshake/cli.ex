defmodule Popcorn.Treeshake.CLI do
  def main(["--apps-dir", apps_dir, "--out-dir", output_dir]) do
    apps = find_apps(apps_dir)
    files = ebin_files(apps)
    owners = module_owners(apps)

    Treeshake.run(ebin_files: files, output_dir: output_dir)
    replace_beams(apps, owners, output_dir)
    rewrite_app_files(apps)
  end

  defp find_apps(apps_dir) do
    apps_dir
    |> Path.join("*/ebin/*.app")
    |> Path.wildcard()
    |> Map.new(fn path -> {Path.basename(path, ".app"), Path.dirname(path)} end)
  end

  defp ebin_files(apps) do
    apps
    |> Map.values()
    |> Enum.flat_map(&Path.wildcard(Path.join(&1, "*.{beam,app}")))
  end

  defp module_owners(apps) do
    apps
    |> Enum.flat_map(fn {app, ebin_dir} ->
      ebin_dir
      |> Path.join("*.beam")
      |> Path.wildcard()
      |> Enum.map(&{beam_module(&1), app})
    end)
    |> Map.new()
  end

  defp replace_beams(apps, owners, output_dir) do
    apps
    |> Map.values()
    |> Enum.each(fn ebin_dir ->
      ebin_dir
      |> Path.join("*.beam")
      |> Path.wildcard()
      |> Enum.each(&File.rm!/1)
    end)

    output_dir
    |> Path.join("*.beam")
    |> Path.wildcard()
    |> Enum.each(fn path ->
      app = Map.fetch!(owners, beam_module(path))
      File.cp!(path, Path.join(apps[app], Path.basename(path)))
    end)
  end

  defp rewrite_app_files(apps) do
    Enum.each(apps, fn {app, ebin_dir} ->
      path = Path.join(ebin_dir, "#{app}.app")
      {:ok, [{:application, name, props}]} = :file.consult(path)

      modules =
        ebin_dir
        |> Path.join("*.beam")
        |> Path.wildcard()
        |> Enum.map(&beam_module/1)
        |> Enum.sort()

      File.write!(
        path,
        :io_lib.format(~c"~p.~n", [{:application, name, Keyword.put(props, :modules, modules)}])
      )
    end)
  end

  defp beam_module(path) do
    {:ok, {module, _chunks}} = :beam_lib.chunks(to_charlist(path), [:exports])
    module
  end
end
