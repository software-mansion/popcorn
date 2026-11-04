defmodule NotebookVM.Application do
  @moduledoc false
  use Application

  # Livebook loads its runtime's code into the VM once it connects, as into
  # the standalone runtime's OS process. See LivebookWasm.Runtime.
  @impl true
  def start(_type, _args) do
    NotebookVM.MixInstall.setup()
    Supervisor.start_link([], strategy: :one_for_one, name: NotebookVM.Supervisor)
  end
end
