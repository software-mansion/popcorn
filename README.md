# Elixir on WASM

Repo gathering efforts for running Elixir in browser using WASM

Repo contains Git submodules, after cloning remember to run

```console
git submodule update --init --recursive
```

## Contents

- `code_deps` - utility for analyzing missing AtomVM NIFs and their deps
- `elixir` - fork of Elixir with examples runnable via WASM
- `ex_doc` - fork of ExDoc for the above
- `fission_lib` - Elixir lib bundling code into `.avm` file for AtomVM
- `fuzzer` - testing AtomVM with fuzzed Erlang code
- `iex_wasm` - IEx in browser via WASM & AtomVM

## First steps

We're using mise to manage tools versions. See the [Getting started](https://mise.jdx.dev/getting-started.html)
Once set up, install required tools with:

```console
mise install
```
