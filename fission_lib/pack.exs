:packbeam_api.create(
  ~c"fission_lib.avm",
  Path.wildcard("out/patched_beam/*.beam") |> Enum.map(&~c"#{&1}")
)

File.cp!("fission_lib.avm", "iex_wasm/avm_deps/fission_lib.avm")
