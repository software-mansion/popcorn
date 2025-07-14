---
title: Popcorn's Architecture
description: Understanding Popcorn's internal architecture and design decisions
---

## Main Architecture

Popcorn runs Elixir code in web browsers by compiling the AtomVM runtime to WebAssembly (Wasm) and executing user code bundle within an isolated iframe environment.

The main browser window hosts your JavaScript code and the Popcorn client JS library. The communication with iframe happens through the `postMessage()` API.

Executing Wasm module in iframe prevents crashes from affecting the main application and enables independent error handling (e.g. restarting the module in case of hangs). The iframe loads the Wasm module which initializes AtomVM and loads your compiled Elixir bytecode.

```mermaid
graph TB
    subgraph "Main browser Window"
        JS[JavaScript application]
        Lib[Popcorn library]
    end

    subgraph "Isolated iframe"
        IFrame[AtomVM runtime and communication code]
        Bundle[Elixir bytecode Bundle]
        Workers@{ shape: processes, label: "Worker threads" }
    end

    JS --> Lib
    Lib <-->|"postMessage()"| IFrame
    Bundle --> IFrame
    IFrame <--> Workers
```

## Patching Mechanism

Popcorn uses a custom patching mechanism to make Elixir and Erlang standard library work with AtomVM's limitations.

The patching system:

1. Takes `.beam` files from known versions of Erlang and Elixir
2. Decompiles them, adding custom patches to work around missing functionality in AtomVM
3. Adds custom modules like `:emscripten` to the standard library
4. Recompiles all of them and bundles into `.avm` bytecode bundle

The patching is currently not exposed to users.

## Runtime Initialization diagram

```mermaid
sequenceDiagram
    participant MW as Main Window
    participant IF as iframe
    participant WM as WASM Module
    participant AVM as AtomVM
    participant EA as Elixir App

    MW->>IF: Create iframe, load scripts
    IF->>WM: Initialize WASM module
    WM->>AVM: Start AtomVM runtime
    AVM->>EA: Load and start application
    EA->>AVM: Wasm.send_elixir_ready()
    AVM->>WM: Elixir ready signal
    WM->>IF: Runtime initialized
    IF->>MW: Ready for communication
```
