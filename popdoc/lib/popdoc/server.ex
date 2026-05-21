defmodule Popdoc.Server do
  @moduledoc false

  def parse_options!(args) do
    {opts, _rest} =
      OptionParser.parse!(args, strict: [port: :integer, dir: :string])

    port = Keyword.get(opts, :port, 4000)
    dir = Path.expand(Keyword.get(opts, :dir, "doc"))

    if not File.dir?(dir) do
      Mix.raise("Directory #{inspect(dir)} does not exist")
    end

    %{dir: dir, port: port}
  end

  def script do
    quote do
      Mix.install([:bandit, :plug])

      defmodule Popdoc.DevServer.Router do
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

      {opts, _rest} =
        OptionParser.parse!(System.argv(), strict: [port: :integer, dir: :string])

      port = Keyword.get(opts, :port, 4000)
      dir = Keyword.fetch!(opts, :dir)

      Application.ensure_all_started(:bandit)

      {:ok, _pid} =
        Supervisor.start_link(
          [
            {Bandit,
             plug: {Popdoc.DevServer.Router, dir}, scheme: :http, port: port, startup_log: false}
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
end
