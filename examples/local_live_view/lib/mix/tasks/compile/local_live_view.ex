defmodule Mix.Tasks.Compile.LocalLiveView do
  use Mix.Task

  @shortdoc "Copies JS files from deps"
  @static_dir "static/local_live_view"
  @out_dir Application.compile_env(:local_live_view, :out_dir, @static_dir)

  def run(_args) do
    deps_js_path =
      [Mix.Project.build_path(), "lib/local_live_view/priv/static/"]
      |> Path.join()

    Mix.shell().cmd("mkdir -p #{@out_dir}")
    Mix.shell().cmd("cp #{deps_js_path}/*.js #{@out_dir}")
    Mix.Tasks.Popcorn.Cook.run(["--out-dir", @out_dir <> "/wasm"])
    :ok
  end
end
