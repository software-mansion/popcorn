# These tests run on a real (native, unix) AtomVM via Popcorn.Support.AtomVM.
# Build the runtime once with: MIX_TARGET=wasm mix popcorn.build_runtime --target unix
Application.put_env(:popcorn, :test_target, :unix)

Application.put_env(
  :popcorn,
  :atomvm_unix_runtime_path,
  Path.join([File.cwd!(), "popcorn_runtime_source/artifacts/unix", "AtomVM"])
)

ExUnit.start()
