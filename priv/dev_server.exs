Mix.install([
  {:bandit, "~> 1.1"},
  {:websock_adapter, "~> 0.5"},
  {:file_system, "~> 0.2.10 or ~> 1.0"}
])

defmodule Reloader do
  def init(_args) do
    FileSystem.subscribe(:live_reload_file_monitor)
    {:ok, []}
  end

  def handle_in({"ping", [opcode: :text]}, state) do
    {:reply, :ok, {:text, "pong"}, state}
  end

  def handle_info({:file_event, _, {path, [:modified, :closed]}}, state) do
    if String.contains?(path, "#{__DIR__}/static") do
      Process.send_after(self(), :reload, 50)
      {:push, {:text, "reload"}, state}
    else
      {:ok, state}
    end
  end

  def handle_info(:reload, state) do
    {:push, {:text, "reload"}, state}
  end

  def handle_info(_, state) do
    {:ok, state}
  end
end

defmodule Router do
  use Plug.Router

  @static_dir "#{__DIR__}/static"

  plug(Plug.Logger)
  plug(:add_headers)
  plug(Plug.Static, from: @static_dir, at: "/")
  plug(:match)
  plug(:dispatch)

  get "/" do
    send_file(conn, 200, "#{@static_dir}/index.html")
  end

  get "app.js" do
    resp = """
    sock = new WebSocket("ws://localhost:#{conn.port}/websocket");
    sock.addEventListener("message", (message) => {
      if (message.data === "reload") {
        window.location.reload();
      }
    });
    sock.addEventListener("open", () => {
      setInterval(() => sock.send("ping"), 10000);
    });
    """

    conn
    |> put_resp_header("content-type", "application/javascript")
    |> send_resp(200, resp)
  end

  get "/websocket" do
    conn
    |> WebSockAdapter.upgrade(Reloader, [], timeout: 60_000)
    |> halt()
  end

  match _ do
    send_resp(conn, 404, "not found")
  end

  def add_headers(conn, _opts) do
    Plug.Conn.merge_resp_headers(
      conn,
      [
        {"Access-Control-Allow-Origin", "*"},
        {"Cross-Origin-Opener-Policy", "same-origin"},
        {"Cross-Origin-Embedder-Policy", "require-corp"}
      ]
    )
  end
end

defmodule Recompiler do
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(_opts) do
    FileSystem.subscribe(:live_reload_file_monitor)
    {:ok, %{}}
  end

  def handle_info({:file_event, _, {path, [:modified, :closed]}}, state) do
    if String.ends_with?(path, ".ex") do
      IO.puts("recompiling #{path}")
      System.cmd("mix", ["popcorn.cook"])
    end

    {:noreply, state}
  end

  def handle_info(_, state) do
    {:noreply, state}
  end
end

{opts, _argv} = OptionParser.parse!(System.argv(), strict: [port: :integer])

bandit = {Bandit, plug: Router, scheme: :http, port: Keyword.get(opts, :port, 4000)}

file_system =
  {FileSystem.Worker, name: :live_reload_file_monitor, dirs: Enum.map([""], &Path.absname/1)}

{:ok, _} = Supervisor.start_link([file_system, bandit, Recompiler], strategy: :one_for_one)

# unless running from IEx, sleep idenfinitely so we can serve requests
unless IEx.started?() do
  IO.getn("Press enter to exit\n")
end
