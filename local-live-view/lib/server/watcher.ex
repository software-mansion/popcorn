defmodule LocalLiveView.Watcher do
  @moduledoc """
  Automatically rebuilds the local project when it changes.

  To use with Phoenix, add to watchers in `config.exs`:

  ```
  config :my_app, MyEndpoint, watchers: [
      local_live_view: {#{inspect(__MODULE__)}, :start_link, []},
      # other watchers
    ]
  ```

  This is automatically added by `mix llv.install`.
  When `phoenix_live_reload` is set up, this enables live reload
  for the local part of the project.
  """

  use GenServer

  require Logger

  @trigger_events [:created, :modified, [:modified, :closed], [:inodemetamod, :modified]]

  @type option :: {:dirs, [Path.t()]}

  @doc """
  Builds the local project once, then starts the watcher.

  This is the entry point used from the endpoint's `:watchers` config, so the
  bundle is up to date before the first request even if the local project
  changed while the server was down.

  ## Options

    * `:dirs` — directories to watch, defaults to `["local"]` relative to the
      project root
  """
  @spec run([option]) :: :ok
  def run(opts \\ []) do
    cook()
    {:ok, _watcher} = start_link(opts)
    :ok
  end

  @doc """
  Starts the watcher without the initial build.

  Takes the same options as `run/1`. Use it when the local project is already
  built and you only want to react to later changes.
  """
  @spec start_link([option]) :: GenServer.on_start()
  def start_link(opts \\ []) do
    opts = Keyword.validate!(opts, dirs: [Path.absname("local")])
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    {:ok, fs_worker} = FileSystem.Worker.start_link(Keyword.take(opts, [:dirs]))
    FileSystem.subscribe(fs_worker)
    {:ok, %{cooking: false, cook_queued: false}}
  end

  @impl true
  def handle_info({:file_event, _pid, {path, event}}, state) do
    trigger_cook = Path.extname(path) == ".ex" and event in @trigger_events

    cond do
      not trigger_cook ->
        {:noreply, state}

      state.cooking ->
        {:noreply, %{state | cook_queued: true}}

      true ->
        cook_async()
        {:noreply, %{state | cooking: true}}
    end
  end

  @impl true
  def handle_info(:cooked, state) do
    # Phoenix doesn't watch for Wasm files by default
    # so we touch a stub JS file.
    File.touch!("priv/static/assets/js/wasm/_reload.js")

    if state.cook_queued do
      cook()
      {:noreply, %{state | cook_queued: false}}
    else
      {:noreply, %{state | cooking: false}}
    end
  end

  @impl true
  def handle_info(_message, state) do
    {:noreply, state}
  end

  defp cook_async() do
    reply_to = self()

    {:ok, _pid} =
      Task.start_link(fn ->
        cook()
        send(reply_to, :cooked)
      end)

    :ok
  end

  defp cook() do
    Logger.debug("Cooking local project")
    {_output, status} = System.shell("mix popcorn.cook", cd: "local")
    if status != 0, do: Logger.warning("Failed to cook local project")
  end
end
