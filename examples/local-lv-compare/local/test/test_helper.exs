# These tests run on a real (native, unix) AtomVM via Popcorn.Support.AtomVM.
# Build the runtime once (e.g. in CI before `mix test`) with:
#
#   MIX_TARGET=wasm mix popcorn.build_runtime --target unix --out-dir test/popcorn_runtime_source
#
Application.put_env(:popcorn, :test_target, :unix)

Application.put_env(
  :popcorn,
  :atomvm_unix_runtime_path,
  Path.join([File.cwd!(), "test/popcorn_runtime_source/artifacts/unix", "AtomVM"])
)

ExUnit.start()
