defmodule LivebookWasm.Application do
  @moduledoc false
  use Application

  # The notebooks' VMs boot a package of their own, see notebook_vm/
  @impl true
  def start(_type, _args) do
    children = [LivebookWasm.Memsup, {Task, &start_livebook/0}]
    Supervisor.start_link(children, strategy: :one_for_one, name: LivebookWasm.Supervisor)
  end

  defp start_livebook do
    started = System.monotonic_time(:millisecond)
    put_base_path()
    {:ok, _apps} = Application.ensure_all_started(:livebook)
    plug = {LivebookWasm.Plug, plug: LivebookWeb.Endpoint}
    {:ok, _pid} = Supervisor.start_child(LivebookWasm.Supervisor, plug)
    ms = System.monotonic_time(:millisecond) - started
    Popcorn.JS.send(%{livebook: "started", ms: ms, node: node()})
  end

  # Each tab's Livebook is under a path of its own, which the page passes, so
  # that each tab has a Service Worker of its own: see public/sw.js
  defp put_base_path do
    with {:ok, [[path]]} <- :init.get_argument(:livebook_wasm_base_path) do
      path = List.to_string(path)
      endpoint = Application.fetch_env!(:livebook, LivebookWeb.Endpoint)
      url = Keyword.put(endpoint[:url], :path, path)
      Application.put_env(:livebook, LivebookWeb.Endpoint, Keyword.put(endpoint, :url, url))
      Application.put_env(:livebook, :iframe_url, path <> "/iframe/v6.html")
    end
  end
end
