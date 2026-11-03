defmodule LivebookWasm.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      LivebookWasm.Memsup,
      {Task, &start_livebook/0}
    ]

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
