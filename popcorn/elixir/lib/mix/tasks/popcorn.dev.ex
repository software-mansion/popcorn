defmodule Mix.Tasks.Popcorn.Dev do
  use Mix.Task

  @shortdoc "Cooks, serves, and re-cooks Popcorn applications on source changes"
  @moduledoc """
  Cooks the current project and serves static files with COOP/COEP headers.

  Accepts all `mix popcorn.cook` options, plus:

    * `--dir` - directory to serve (default: `public`)
    * `--port` - HTTP port (default: `4000`)

  On Linux, file watching requires `inotify-tools`.

      mix popcorn.dev
      mix popcorn.dev --dir www --out-dir www/out --port 8080
  """

  defmacrop server_script do
    quote do
      :io.setopts(:standard_io, encoding: :unicode)

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
          if conn.request_path == "/" and File.regular?(Path.join(dir, "index.html")) do
            conn
            |> Plug.Conn.put_resp_content_type("text/html")
            |> Plug.Conn.send_file(200, Path.join(dir, "index.html"))
          else
            Plug.Conn.send_resp(conn, 404, "not found")
          end
        end
      end

      flag_config = [strict: [port: :integer, dir: :string]]
      {opts, _} = OptionParser.parse!(System.argv(), flag_config)

      port = Keyword.get(opts, :port, 4000)
      dir = Keyword.fetch!(opts, :dir)

      Application.ensure_all_started(:bandit)

      bandit = [
        plug: {Popcorn.DevServer.Router, dir},
        scheme: :http,
        port: port,
        startup_log: false
      ]

      {:ok, _} = Supervisor.start_link([{Bandit, bandit}], strategy: :one_for_one)

      IO.puts("Serving #{Path.relative_to(dir, File.cwd!())}")
      IO.puts("Popcorn dev server: http://localhost:#{port}")

      IO.read(:stdio, :eof)
    end
    |> Macro.to_string()
  end

  @impl true
  def run(argv) do
    flag_config = [strict: Mix.Tasks.Popcorn.Cook.switches() ++ [dir: :string, port: :integer]]
    {options, arguments} = OptionParser.parse!(argv, flag_config)

    if not Enum.empty?(arguments) do
      Mix.raise("Unexpected arguments: #{Enum.join(arguments, " ")}")
    end

    cook_args = options |> Keyword.drop([:dir, :port]) |> OptionParser.to_argv()
    dir = options |> Keyword.get(:dir, "public") |> Path.expand()
    out_dir = options |> Keyword.get(:out_dir, "public/out") |> Path.expand()
    port = Keyword.get(options, :port, 4000)

    Mix.Task.run("deps.loadpaths")
    roots = [File.cwd!() | path_dependencies()]
    sources = source_paths(roots)
    Mix.shell().info("Cooking Popcorn...")
    if cook(cook_args) != 0, do: Mix.raise("Initial Popcorn cook failed")
    File.mkdir_p!(dir)

    server =
      Port.open({:spawn_executable, System.find_executable("elixir")}, [
        :binary,
        :exit_status,
        :stderr_to_stdout,
        args: [
          "--erl",
          "+Bi",
          "-e",
          server_script(),
          "--",
          "--dir",
          dir,
          "--port",
          to_string(port)
        ]
      ])

    {:ok, _apps} = Application.ensure_all_started(:file_system)
    {:ok, watcher} = FileSystem.start_link(dirs: roots)
    :ok = FileSystem.subscribe(watcher)

    try do
      watch(server, watcher, sources, out_dir, cook_args, false)
    after
      GenServer.stop(watcher)
      if Port.info(server), do: Port.close(server)
    end
  end

  defp path_dependencies do
    for dep <- Mix.Dep.cached(), dep.scm == Mix.SCM.Path do
      Keyword.fetch!(dep.opts, :dest)
    end
  end

  defp source_paths(roots) do
    config = Mix.Project.config()

    dirs =
      Keyword.get(config, :elixirc_paths, ["lib"]) ++
        Keyword.get(config, :erlc_paths, ["src"]) ++ ["config", "priv", "include"]

    for root <- roots, path <- dirs ++ ["mix.exs", "mix.lock"], do: Path.join(root, path)
  end

  defp source?(path, sources, out_dir) do
    not under?(path, out_dir) and Enum.any?(sources, &under?(path, &1))
  end

  defp under?(path, dir), do: path == dir or String.starts_with?(path, dir <> "/")

  defp cook(args) do
    {output, status} =
      System.cmd(System.find_executable("mix"), ["popcorn.cook" | args],
        stderr_to_stdout: true,
        env: [{"MIX_ENV", to_string(Mix.env())}, {"MIX_TARGET", to_string(Mix.target())}]
      )

    IO.write(output)
    status
  end

  defp watch(server, watcher, sources, out_dir, cook_args, pending) do
    receive do
      {^server, {:data, data}} ->
        IO.write(data)
        watch(server, watcher, sources, out_dir, cook_args, pending)

      {^server, {:exit_status, status}} ->
        Mix.raise("Popcorn development server exited with status #{status}")

      {:file_event, ^watcher, {path, _events}} ->
        pending =
          if !pending and source?(path, sources, out_dir) do
            Process.send_after(self(), :rebuild, 200)
            true
          else
            pending
          end

        watch(server, watcher, sources, out_dir, cook_args, pending)

      {:file_event, ^watcher, :stop} ->
        Mix.raise("Popcorn source watcher stopped")

      :rebuild ->
        Mix.shell().info("Sources changed; rebuilding Popcorn...")

        case cook(cook_args) do
          0 -> Mix.shell().info("Popcorn rebuilt. Refresh the browser to load changes.")
          _status -> Mix.shell().error("Popcorn rebuild failed; waiting for source changes")
        end

        watch(server, watcher, sources, out_dir, cook_args, false)
    end
  end
end
