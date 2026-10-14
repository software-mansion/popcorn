defmodule LocalLiveView.Watcher do
  @moduledoc """
  Automatically rebuilds the local project when it changes.

  To use with Phoenix, add to watchers in `config.exs`:

  ```
  config :my_app, MyEndpoint, watchers: [
      local_live_view: {#{inspect(__MODULE__)}, :run, []},
      # other watchers
    ]
  ```

  This is automatically added by `mix llv.install`.
  When `phoenix_live_reload` is set up, this enables live reload
  for the local part of the project.
  """

  use GenServer

  require Logger

  # A single save can emit a few events, e.g. MODIFY and CLOSE_WRITE on Linux
  @debounce_ms 100

  @type option :: {:dirs, [Path.t()]}

  @doc """
  Starts the watcher, which builds the local project right away, in case it
  changed while the server was down, and then whenever it changes.

  This is the entry point used from the endpoint's `:watchers` config.

  ## Options

    * `:dirs` — directories to watch, defaults to `["local/lib"]` relative to
      the project root
  """
  @spec run([option]) :: :ok
  def run(opts \\ []) do
    {:ok, _watcher} = start_link(opts)
    :ok
  end

  @doc """
  Same as `run/1`, but returns the watcher's pid.
  """
  @spec start_link([option]) :: GenServer.on_start()
  def start_link(opts \\ []) do
    opts = Keyword.validate!(opts, dirs: [Path.absname("local/lib")])
    GenServer.start_link(__MODULE__, opts)
  end

  @impl true
  def init(opts) do
    case FileSystem.Worker.start_link(Keyword.take(opts, [:dirs])) do
      {:ok, fs_worker} ->
        FileSystem.subscribe(fs_worker)

      :ignore ->
        Logger.warning("Can't watch the local project, it will only be built once")
    end

    ref = make_ref()
    send(self(), {:cook, ref})
    {:ok, %{cook_ref: ref}}
  end

  # Any event counts: edits, deletions and renames all change the build, and a
  # touch forces a rebuild, like it does with `mix compile`. Hidden files are
  # skipped, as they're editors' temp files, like Vim's swap files.
  @impl true
  def handle_info({:file_event, _pid, {path, _events}}, state) do
    if String.starts_with?(Path.basename(path), ".") do
      {:noreply, state}
    else
      # Debounced: only the timer of the latest event cooks
      ref = make_ref()
      Process.send_after(self(), {:cook, ref}, @debounce_ms)
      {:noreply, %{state | cook_ref: ref}}
    end
  end

  # Cooks synchronously, so changes made in the meantime wait in the mailbox
  # and trigger another cook afterwards
  def handle_info({:cook, ref}, %{cook_ref: ref} = state) do
    cook()
    {:noreply, %{state | cook_ref: nil}}
  end

  def handle_info(_message, state) do
    {:noreply, state}
  end

  defp cook() do
    Logger.debug("Cooking local project")

    # In the local project, llv.build only cooks it
    case System.cmd("mix", ["llv.build"], cd: "local", into: IO.stream(), stderr_to_stdout: true) do
      {_output, 0} ->
        # Phoenix doesn't watch for Wasm files by default
        # so we touch a stub JS file.
        File.touch!(Path.join(Mix.Tasks.Llv.Build.out_dir(), "_reload.js"))

      _error ->
        Logger.warning("Failed to cook local project")
    end
  end
end
