# Limitations

Popcorn is a work-in-progress state. While functional, it has several limitations described below. Some of them are inherent for the technology used (AtomVM, browser, Wasm). However, other limitations will disappear in the future.

The Popcorn API is **unstable** and may change between versions. We are very early in the process of API design.

If you feel you encountered a bug or you need a missing feature, please visit [GitHub repository](https://github.com/software-mansion/popcorn) and report it there.

## AtomVM runtime limitations

Popcorn relies on [AtomVM](https://www.atomvm.net/) to execute compiled Elixir bytecode in the browser. AtomVM is designed for microcontrollers in mind. It doesn't implement the entire OTP standard library and lacks support for newer or rarely used features of the BEAM.

Examples:

- **Big integers**: Not fully supported (work in progress on AtomVM side)
- **Bitstrings**: Limited functionality when matching on non-octet offsets
- **Distributed Erlang**: Not yet downstreamed, beta feature of AtomVM

## Missing standard library functions

Pure Erlang and Elixir parts of the standard library work well. Some parts of it uses natively implemented functions (NIFs) which VM must implement. AtomVM doesn't implement all NIFs. For example, `:ets`, `:logger`, `:timer`, `:rand`, and other have limited support.

Popcorn works around it by patching the functions using NIFs and provides pure Erlang implementation. However, this approach doesn't work for all NIFs. We work with AtomVM maintainers to implement missing NIFs.

## Iframe

Popcorn runs in an iframe to isolate the main window from crashes. This means that it is subject to iframe security restrictions. We expect existence of bugs related to communication with main window and using browser APIs. This should improve with time.

## JavaScript interoperability constraints

All communication between Elixir and JavaScript uses JSON serialization and goes through the iframe `postMessage()` API. This introduces latencies when calling across languages. We try to avoid it by batching calls and minimalizing serialization.

Additionally, not all values can be transferred between JavaScript and Elixir:

- Only JSON-serializable JS and Elixir values can be sent between runtimes
- Complex JS objects require passing opaque references back to Elixir

## Limited Debugging Tools

- Standard Elixir debugging tools are not available
- Error messages may be less detailed than native Elixir
