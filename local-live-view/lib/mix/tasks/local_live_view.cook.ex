defmodule Mix.Tasks.LocalLiveView.Cook do
  use Mix.Task

  @stubs_dir "stubs"
  @stubs_out "_build/stubs"

  @impl true
  def run(_args) do
    Mix.Task.run("compile")
    Mix.Task.run("app.config")

    # 1. Compile stub modules and write their .beam files
    stub_modules = compile_stubs()
    stub_module_set = MapSet.new(stub_modules, fn {mod, _} -> "#{mod}" end)

    # 2. Collect beams from runtime: false deps, excluding those overridden by stubs
    dep_beams =
      ~w[phoenix phoenix_live_view phoenix_html phoenix_template phoenix_ecto ecto plug]
      |> Enum.flat_map(fn dep ->
        Path.wildcard(Path.join([Mix.Project.build_path(), "lib", dep, "ebin", "*.beam"]))
      end)
      |> Enum.reject(fn beam_path ->
        module_name = Path.basename(beam_path, ".beam")
        MapSet.member?(stub_module_set, module_name)
      end)

    # 3. Add stub beams
    stub_beams = Path.wildcard(Path.join([@stubs_out, "*.beam"]))

    Popcorn.cook(extra_beams: dep_beams ++ stub_beams)
  end

  defp compile_stubs do
    File.mkdir_p!(@stubs_out)
    stub_files = Path.wildcard(Path.join([@stubs_dir, "*.ex"]))

    all_modules =
      for file <- stub_files, reduce: [] do
        acc ->
          purge_stubbed_modules(file)
          modules = Code.compile_file(file)

          for {mod, binary} <- modules do
            beam_path = Path.join(@stubs_out, "#{mod}.beam")
            File.write!(beam_path, binary)
          end

          acc ++ modules
      end

    all_modules
  end

  defp purge_stubbed_modules(file) do
    {:ok, content} = File.read(file)
    {:ok, ast} = Code.string_to_quoted(content)

    modules =
      Macro.prewalk(ast, [], fn
        {:defmodule, _, [{:__aliases__, _, name} | _]} = node, acc ->
          {node, [Module.concat(name) | acc]}

        {:defmodule, _, [name | _]} = node, acc when is_atom(name) ->
          {node, [name | acc]}

        node, acc ->
          {node, acc}
      end)
      |> elem(1)

    for mod <- modules do
      :code.which(mod)
      |> to_string()
      |> File.rm()
    end
  end
end
