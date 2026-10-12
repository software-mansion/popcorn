import ExUnit.Assertions

Logger.configure(level: :warning)
assert Code.ensure_loaded?(Req)
assert :ok = Application.load(:req)
Code.compile_string("defmodule :wasm do\n  def run_js(_, _, _), do: nil\nend")
assert Popcorn.Wasm.available?()

for opts <- [[receive_timeout: 1_000], [adapter: CustomAdapter, receive_timeout: 1_000]] do
  Application.put_env(:req, :default_options, opts)
  assert {:ok, supervisor} = Popcorn.Application.start(:normal, [])

  expected = Keyword.put_new(opts, :adapter, Popcorn.Fetch.adapter())
  assert Application.get_env(:req, :default_options) == expected
  assert is_pid(Process.whereis(:popcorn_proxy))
  Supervisor.stop(supervisor)
end
