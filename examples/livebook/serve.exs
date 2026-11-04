# Hosts public/, as cooked by `mix popcorn.dev`, the way a static host would.
# Unlike `mix popcorn.dev`, it serves the shell page for Livebook's links too,
# like /import?url=…
#
# Usage: elixir serve.exs [port]

Mix.install([{:bandit, "~> 1.0"}, {:plug, "~> 1.14"}])

defmodule Serve do
  @behaviour Plug
  import Plug.Conn

  @public Path.expand("public", __DIR__)

  # Popcorn's VMs need the page to be cross-origin isolated
  @headers [
    {"cross-origin-opener-policy", "same-origin"},
    {"cross-origin-embedder-policy", "require-corp"}
  ]

  # Livebook's assets are only there gzipped
  @static Plug.Static.init(
            at: "/",
            from: @public,
            brotli: true,
            gzip: true,
            cache_control_for_etags: "no-cache"
          )

  @impl true
  def init(opts), do: opts

  @impl true
  def call(conn, _opts) do
    conn
    |> merge_resp_headers(@headers)
    |> Plug.Static.call(@static)
    |> page()
  end

  defp page(%{halted: true} = conn), do: conn

  # Livebook's links open the shell page, which passes them to Livebook. The
  # tab's own paths are its Service Worker's.
  defp page(conn) do
    page? = conn |> get_req_header("accept") |> Enum.any?(&String.contains?(&1, "text/html"))

    if page? and not match?(["tab" | _], conn.path_info) do
      conn
      |> put_resp_header("cache-control", "no-cache")
      |> put_resp_content_type("text/html")
      |> send_file(200, Path.join(@public, "index.html"))
    else
      send_resp(conn, 404, "not found")
    end
  end
end

port =
  case System.argv() do
    [port] -> String.to_integer(port)
    [] -> 4001
  end

{:ok, server} = Bandit.start_link(plug: Serve, port: port, startup_log: false)
{:ok, {_address, port}} = ThousandIsland.listener_info(server)
IO.puts("Serving #{Path.expand("public", __DIR__)} at http://localhost:#{port}")
Process.sleep(:infinity)
