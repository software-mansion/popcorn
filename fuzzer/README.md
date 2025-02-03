# Fuzzing AtomVM

## Requirements

- `cargo` for compiling Erlfuzz
- `erlc` for compiling Erlang files
- `parallel` for fuzzing process (`brew install parallel`, `parallel --citation` to agree to GNU terms on using parallel)
- AtomVM binary
- PackBeam binary for creating .avm files
- `.avm` with patched standard library (`fission_lib.avm`)

## Setup

- `cargo build --release` in `erlfuzz/`
- create `.env` file with three environment variables containing absolute paths to needed binaries
  - `AVM_ERLFUZZ_BIN`
  - `AVM_ATOM_VM_BIN`
  - `AVM_PACKBEAM_BIN`
  - `AVM_FISSION_LIB_AVM`

Example .env file:

```bash
AVM_ERLFUZZ_BIN="/Users/USER/dev/elixir-wasm/fuzzer/erlfuzz/target/release/erlfuzz"
AVM_ATOM_VM_BIN="/Users/USER/dev/AtomVM/build/src/AtomVM"
AVM_PACKBEAM_BIN="/Users/USER/dev/AtomVM/build/tools/packbeam/PackBEAM"
AVM_FISSION_LIB_AVM="/Users/USER/dev/elixir-wasm/fission_lib/_build/dev/lib/fission_lib/fission_lib.avm"
```

## Running

```bash
./run.sh 1000 # number of samples generated
```
