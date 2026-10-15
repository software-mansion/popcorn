import ExUnit.Assertions

refute Code.ensure_loaded?(Req)

root = Path.expand("../..", __DIR__)

{modules, diagnostics} =
  Code.with_diagnostics(fn ->
    Enum.flat_map(
      [
        "lib/popcorn/js.ex",
        "lib/popcorn/fetch.ex",
        "lib/popcorn/proxy.ex",
        "lib/popcorn/application.ex",
        "test/support/fake_bridge.ex"
      ],
      &Code.compile_file(Path.join(root, &1))
    )
  end)

assert diagnostics == []
refute Code.ensure_loaded?(Req)
assert Popcorn.Fetch.__info__(:functions) == [request: 1, request: 2]

for {module, beam} <- modules, module in [Popcorn.Fetch, Popcorn.Application] do
  assert {:ok, {^module, [atoms: atoms, imports: imports]}} =
           :beam_lib.chunks(beam, [:atoms, :imports])

  refute Enum.any?(atoms, fn {_index, atom} ->
           String.starts_with?(to_string(atom), "Elixir.Req")
         end)

  refute Enum.any?(imports, fn {module, _function, _arity} ->
           String.starts_with?(to_string(module), "Elixir.Req")
         end)
end

alias Popcorn.JS.FakeBridge
Application.put_env(:popcorn, :wasm_bridge, FakeBridge)

request = %{method: "POST", url: "/echo", body: <<255, 0, 65, 254>>}

FakeBridge.stub_fetch(%{
  status: 201,
  headers: [{"x-fetch", "plain"}],
  chunks: [<<255, 0>>, <<65, 254>>]
})

assert Popcorn.Fetch.request(request) ==
         {:ok, %{status: 201, headers: [{"x-fetch", "plain"}], body: request.body}}

assert Base.decode64!(FakeBridge.fetch_request().body) == request.body

FakeBridge.stub_fetch(:never_replies)
assert Popcorn.Fetch.request(request, timeout: 0) == {:error, :timeout}
assert FakeBridge.fetch_aborted?()

Code.compile_string("defmodule :wasm do\n  def run_js(_, _, _), do: nil\nend")
assert Popcorn.JS.available?()
assert {:ok, supervisor} = Popcorn.Application.start(:normal, [])
assert is_pid(Process.whereis(:popcorn_proxy))
assert Application.get_env(:req, :default_options) == nil
Supervisor.stop(supervisor)
