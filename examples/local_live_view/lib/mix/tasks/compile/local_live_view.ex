defmodule Mix.Tasks.Compile.LocalLiveView do
  use Mix.Task

  @shortdoc "Copies JS files from deps to static/assets"

  def run(_args) do
    deps_js_path =
      [Mix.Project.build_path(), "lib/local_live_view/priv/static/"]
      |> Path.join
    dest_path = "static/assets"
    Mix.shell().cmd("mkdir -p #{dest_path}")
    Mix.shell().cmd("cp #{deps_js_path}/*.js #{dest_path}")
    :ok
  end
end