Mix.install([
  {:bandit, "~> 1.1"}
])

defmodule Router do
  use Plug.Router

  @static_dir "#{__DIR__}/static"

  plug(Plug.Logger)
  plug(:add_headers)
  plug(Plug.Static, from: @static_dir, at: "/", gzip: true)
  plug(:match)
  plug(:dispatch)

  get "/" do
    send_file(conn, 200, "#{@static_dir}/index.html")
  end

  get "/erlang" do
    send_file(conn, 200, "#{@static_dir}/erlang.html")
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

{opts, _argv} = OptionParser.parse!(System.argv(), strict: [port: :integer])

bandit = {Bandit, plug: Router, scheme: :http, port: Keyword.get(opts, :port, 4000)}
{:ok, _} = Supervisor.start_link([bandit], strategy: :one_for_one)

# unless running from IEx, sleep idenfinitely so we can serve requests
unless IEx.started?() do
  IO.getn("Press enter to exit\n")
end
