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
    # Each tab's, which the notebooks' VMs get too: tabs' VMs can connect to
    # each other's, see public/distribution.js
    Application.put_env(:livebook, :cookie, Livebook.Utils.random_cookie())
    {:ok, _apps} = Application.ensure_all_started(:livebook)
    star_examples()
    plug = {LivebookWasm.Plug, plug: LivebookWeb.Endpoint}
    {:ok, _pid} = Supervisor.start_child(LivebookWasm.Supervisor, plug)
    ms = System.monotonic_time(:millisecond) - started
    Popcorn.JS.send(%{livebook: "started", ms: ms, node: node()})
  end

  # The example notebooks, in the order of their cards on the home page
  @examples ~w(charts maps processes nx nodes testing)

  # Livebook's home page shows starred notebooks first, so the examples are
  # starred. They're copied to the home directory, so that editing them leaves
  # the package's as they are. Livebook adds each to the front of the list.
  defp star_examples do
    dir = Path.join(System.user_home!(), "examples")
    File.mkdir_p!(dir)

    for name <- Enum.reverse(@examples) do
      path = Path.join(dir, "#{name}.livemd")
      File.cp!(Path.join([:code.priv_dir(:livebook_wasm), "notebooks", "#{name}.livemd"]), path)
      "# " <> title = path |> File.stream!() |> Enum.at(0) |> String.trim()
      Livebook.NotebookManager.add_starred_notebook(Livebook.FileSystem.File.local(path), title)
    end
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
