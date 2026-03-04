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

end
