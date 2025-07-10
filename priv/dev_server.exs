Mix.install([
  {:bandit, "~> 1.1"},
  {:websock_adapter, "~> 0.5"},
  {:file_system, "~> 1.0"}
])

defmodule Reloader do
  require Logger

  def init(_args) do
    Registry.register(Dispatcher, :reload, [])
    {:ok, []}
  end

  def handle_in({"ping", [opcode: :text]}, state) do
    {:reply, :ok, {:text, "pong"}, state}
  end

  def handle_info(:reload, state) do
    Logger.debug("mix popcorn.cook finished. Reloading page.")
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

  get "dev_server.js" do
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
  require Logger
  use GenServer

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(_opts) do
    FileSystem.subscribe(:live_reload_file_monitor)
    {:ok, %{cooking: false, cook_queued: false, cook_ref: nil}, {:continue, :cook}}
  end

  def handle_continue(:cook, state) do
    {:noreply, cook(state)}
  end

  @valid_events [:created, :modified, [:modified, :closed], [:inodemetamod, :modified]]
  def handle_info({:file_event, _pid, {path, event}}, state) do
    valid = String.ends_with?(path, ".ex") && event in @valid_events

    state =
      case {valid, state.cooking} do
        {false, _} -> state
        {true, true} -> %{state | cook_queued: true}
        {true, false} -> cook(state)
      end

    {:noreply, state}
  end

  def handle_info({cook_ref, _value}, %{cook_ref: cook_ref} = state) do
    Registry.dispatch(Dispatcher, :reload, fn entries ->
      for {pid, _value} <- entries, do: send(pid, :reload)
    end)

    state =
      if state.cook_queued do
        cook(state)
      else
        %{state | cooking: false, cook_ref: nil}
      end

    {:noreply, state}
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  defp cook(state) do
    Logger.debug("running mix popcorn.cook")
    %{ref: cook_ref} = Task.async(fn -> System.cmd("mix", ["popcorn.cook"]) end)
    %{state | cook_queued: false, cooking: true, cook_ref: cook_ref}
  end
end

{opts, _argv} = OptionParser.parse!(System.argv(), strict: [port: :integer])

bandit = {Bandit, plug: Router, scheme: :http, port: Keyword.get(opts, :port, 4000)}

file_system =
  {FileSystem.Worker, name: :live_reload_file_monitor, dirs: Enum.map(["."], &Path.absname/1)}

children = [{Registry, keys: :duplicate, name: Dispatcher}, file_system, bandit, Recompiler]
{:ok, _} = Supervisor.start_link(children, strategy: :one_for_one)

# unless running from IEx, sleep idenfinitely so we can serve requests
unless IEx.started?() do
  IO.getn("Press enter to exit\n")
end
