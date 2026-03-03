Mix.install([:bandit, :plug])

defmodule MyPlug do
  use Plug.Builder

  plug Plug.Static,
    at: "/bundle", 
    from: Path.join([__DIR__, "..", "_build", "dev"]),
    only: ~w"bundle.avm"
  plug Plug.Static,
    at: "/", 
    from: __DIR__,
    headers: [
      {"Cross-Origin-Opener-Policy", "same-origin"},
      {"Cross-Origin-Embedder-Policy","require-corp"}
    ]
  plug :not_found

  def not_found(conn, _) do
    send_resp(conn, 404, "not found")
  end
end

{:ok, _} = Supervisor.start_link([{Bandit, plug: MyPlug}], strategy: :one_for_one)
Process.sleep(:infinity)

