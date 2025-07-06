Mix.install([
  {:bandit, "~> 1.1"}
])

defmodule Router do
  use Plug.Router
  plug(Plug.Logger)
  plug(:add_headers)
  plug(Plug.Static, from: ".", at: "/")
  plug(:match)
  plug(:dispatch)

  get "/" do
    send_file(conn, 200, "index.html")
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

bandit = {Bandit, plug: Router, scheme: :http, port: 4000}
{:ok, _} = Supervisor.start_link([bandit], strategy: :one_for_one)

# unless running from IEx, sleep idenfinitely so we can serve requests
unless IEx.started?() do
  Process.sleep(:infinity)
end
