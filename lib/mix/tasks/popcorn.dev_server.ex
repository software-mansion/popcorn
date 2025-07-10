defmodule Mix.Tasks.Popcorn.DevServer do
  @shortdoc """
  Generates a static file server script with code reloading.
  """
  @moduledoc """
  #{@shortdoc}

  The dev server is more complicated than the simple server as
  sets up a filesystem watcher and reloads the browser window
  when the popcorn bundle changes.

  It requires the following line in the index.html:

  ```javascript
  <script type="text/javascript" src="/app.js"></script>
  ```

  These assets can also be hosted with any other server,
  but it has to add the following HTTP headers:

  ```txt
  Cross-Origin-Opener-Policy: same-origin
  Cross-Origin-Embedder-Policy: require-corp
  ```

  Otherwise the browser refuses to run WASM.
  """
  @shortdoc @moduledoc

  use Mix.Task

  @priv_dir :code.priv_dir(:popcorn)

  @impl true
  def run(_args) do
    File.cp!(Path.join(@priv_dir, "dev_server.exs"), "dev_server.exs")
  end
end
