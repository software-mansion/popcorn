defmodule Mix.Tasks.Popcorn.Server do
  @shortdoc "Starts a static file server with COOP/COEP headers for running WASM."
  @moduledoc """
  #{@shortdoc}

  Serves files from the configured directory with the HTTP headers
  required by browsers to run WebAssembly with `SharedArrayBuffer`.

  ## Options

    * `--port` - port to listen on (default: 4000)
    * `--dir` - directory to serve files from (default: "static")

  ## Examples

      $ mix popcorn.server
      $ mix popcorn.server --port 8080 --dir dist
  """

  use Mix.Task

  @impl true
  def run(args) do
    {opts, _rest} =
      OptionParser.parse!(args, strict: [port: :integer, dir: :string])

    port = Keyword.get(opts, :port, 4000)
    dir = Path.expand(Keyword.get(opts, :dir, "static"))

    unless File.dir?(dir) do
      Mix.raise("Directory #{inspect(dir)} does not exist")
    end

    Application.ensure_all_started(:bandit)

    bandit =
      {Bandit, plug: {__MODULE__.Router, dir}, scheme: :http, port: port, startup_log: false}

    {:ok, _} = Supervisor.start_link([bandit], strategy: :one_for_one)

    project_root = Path.basename(File.cwd!())
    relative_dir = Path.relative_to(dir, File.cwd!())
    Mix.shell().info("Serving #{project_root}/#{relative_dir} at http://localhost:#{port}")

    unless IEx.started?() do
      Process.sleep(:infinity)
    end
  end

  defmodule Router do
    @moduledoc false
    @behaviour Plug

    @headers [
      {"access-control-allow-origin", "*"},
      {"cache-control", "public no-cache"},
      {"cross-origin-opener-policy", "same-origin"},
      {"cross-origin-embedder-policy", "require-corp"}
    ]

    @impl true
    def init(static_dir) do
      %{
        static_dir: static_dir,
        static: Plug.Static.init(from: static_dir, at: "/", gzip: true),
        logger: Plug.Logger.init([])
      }
    end

    @impl true
    def call(conn, %{static_dir: dir, static: static, logger: logger}) do
      conn
      |> Plug.Logger.call(logger)
      |> Plug.Conn.merge_resp_headers(@headers)
      |> Plug.Static.call(static)
      |> serve(dir)
    end

    defp serve(%{halted: true} = conn, _dir), do: conn

    defp serve(conn, dir) do
      path =
        case conn.request_path do
          "/" -> "index.html"
          other -> String.trim_leading(other, "/") <> ".html"
        end

      file = Path.join(dir, path)

      if File.exists?(file) do
        Plug.Conn.send_file(conn, 200, file)
      else
        Plug.Conn.send_resp(conn, 404, "not found")
      end
    end
  end
end
