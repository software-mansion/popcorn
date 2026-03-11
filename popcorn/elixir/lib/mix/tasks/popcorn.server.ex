defmodule Mix.Tasks.Popcorn.Server do
  @shortdoc "Starts a static file server with COOP/COEP headers for running WASM."
  @moduledoc """
  #{@shortdoc}

  Serves files from the configured directory with the HTTP headers
  required by browsers to run WebAssembly with `SharedArrayBuffer`.

  The server runs as a separate Elixir process using `Mix.install` to
  fetch Bandit and Plug, so these dependencies don't need to be in your
  project's `mix.exs`.

  ## Options

    * `--port` - port to listen on (default: 4000)
    * `--dir` - directory to serve files from (default: "dist")

  ## Examples

      $ mix popcorn.server
      $ mix popcorn.server --port 8080 --dir dist
  """

  use Mix.Task

  defmacrop server_script do
    quote do
      Mix.install([:bandit, :plug])

      defmodule Popcorn.DevServer.Router do
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

      {opts, _} =
        OptionParser.parse!(System.argv(), strict: [port: :integer, dir: :string])

      port = Keyword.get(opts, :port, 4000)
      dir = Keyword.fetch!(opts, :dir)

      Application.ensure_all_started(:bandit)

      {:ok, _} =
        Supervisor.start_link(
          [
            {Bandit,
             plug: {Popcorn.DevServer.Router, dir}, scheme: :http, port: port, startup_log: false}
          ],
          strategy: :one_for_one
        )

      IO.puts(
        "Serving #{Path.basename(File.cwd!())}/#{Path.relative_to(dir, File.cwd!())} at http://localhost:#{port}"
      )

      IO.read(:stdio, :eof)
    end
    |> Macro.to_string()
  end

  @impl true
  def run(args) do
    {opts, _rest} =
      OptionParser.parse!(args, strict: [port: :integer, dir: :string])

    port_num = Keyword.get(opts, :port, 4000)
    dir = Path.expand(Keyword.get(opts, :dir, "dist"))

    if not File.dir?(dir) do
      Mix.raise("Directory #{inspect(dir)} does not exist")
    end

    elixir = System.find_executable("elixir")
    if is_nil(elixir), do: Mix.raise("elixir executable not found")

    elixir_args = [
      "--erl",
      "+Bi",
      "-e",
      server_script(),
      "--",
      "--port",
      to_string(port_num),
      "--dir",
      dir
    ]

    {:spawn_executable, elixir}
    |> Port.open([
      :binary,
      :exit_status,
      :stderr_to_stdout,
      args: elixir_args
    ])
    |> forward_output()
  end

  defp forward_output(child) do
    receive do
      {^child, {:data, data}} ->
        IO.write(data)
        forward_output(child)

      {^child, {:exit_status, _}} ->
        :ok
    end
  end
end
