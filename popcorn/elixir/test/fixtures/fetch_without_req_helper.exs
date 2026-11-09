import ExUnit.Assertions

Logger.configure(level: :warning)
refute Code.ensure_loaded?(Req)

root = Path.expand("../..", __DIR__)

{modules, diagnostics} =
  Code.with_diagnostics(fn ->
    Enum.flat_map(
      [
        "lib/popcorn/js.ex",
        "lib/popcorn/fetch.ex",
        "lib/popcorn/fetch/req_adapter.ex",
        "lib/popcorn/proxy.ex",
        "lib/popcorn/application.ex",
        "test/support/fake_bridge.ex"
      ],
      &Code.compile_file(Path.join(root, &1))
    )
  end)

assert diagnostics == []
refute Code.ensure_loaded?(Req)
assert Popcorn.Fetch.ReqAdapter in Enum.map(modules, &elem(&1, 0))

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

# Req comes later, as from Mix.install/2, whose patch installs the adapter then
[req_dir] = System.argv()
Code.prepend_path(Path.join(req_dir, "ebin"))
assert :ok = Application.load(:req)
assert :ok = Popcorn.Fetch.install_req_adapter()
assert Application.get_env(:req, :default_options) == [adapter: Popcorn.Fetch.adapter()]
