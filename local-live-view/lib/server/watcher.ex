defmodule LocalLiveView.Watcher do
  @moduledoc """
  Automatically rebuilds the local project when it changes.

  To use with Phoenix, add to watchers in `config.exs`:

  ```
  confing :my_app, MyEndpoint, watchers: [
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

  @spec run([option]) :: :ok
  def run(opts \\ []) do
    cook()
    {:ok, _watcher} = start_link(opts)
    :ok
  end

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
    trigger_cook = String.ends_with?(path, ".ex") and event in @trigger_events

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
    # Phoenix doesn't watch for WASM files by default
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
