defmodule LivebookWasm.Application do
  @moduledoc false
  use Application

  # The page boots Livebook's VM, and a VM for each notebook's runtime, with
  # `-livebook_wasm_role runtime`. See LivebookWasm.Runtime.
  @impl true
  def start(_type, _args) do
    children =
      case :init.get_argument(:livebook_wasm_role) do
        {:ok, [[~c"runtime"]]} -> [{Task, &LivebookWasm.MixInstall.setup/0}]
        _livebook -> [LivebookWasm.Memsup, {Task, &start_livebook/0}]
      end

    Supervisor.start_link(children, strategy: :one_for_one, name: LivebookWasm.Supervisor)
  end

  defp start_livebook do
    started = System.monotonic_time(:millisecond)
    {:ok, _apps} = Application.ensure_all_started(:livebook)
    plug = {Popcorn.Plug, plug: LivebookWeb.Endpoint}
    {:ok, _pid} = Supervisor.start_child(LivebookWasm.Supervisor, plug)
    ms = System.monotonic_time(:millisecond) - started
    Popcorn.JS.send(%{livebook: "started", ms: ms, node: node()})
  end
end
