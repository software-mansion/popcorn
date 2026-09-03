# Popcorn

Popcorn runs Elixir and Erlang applications in the browser, with a JavaScript API for communication.

It uses the BEAM virtual machine from Erlang/OTP, compiled to WebAssembly.
Your application runs on the user's device, with BEAM processes, message passing, and supervision.

## How it works

The Vite, Rollup, and esbuild plugins package your compiled application and its dependencies as browser assets.
Your Elixir and Erlang modules keep their BEAM bytecode format. The VM runs separately from the page's JavaScript thread.

JavaScript sends messages to BEAM processes and receives their replies through the bridge.
Elixir can also request JavaScript execution in the page, where browser APIs such as the DOM are available.
Browser restrictions still apply to the VM, including limits on operating-system access and native libraries.

## APIs

- [JavaScript API](JS.Popcorn.html): start and stop the VM, send messages, and connect terminal input and output.
- `Popcorn.Wasm`: send messages to JavaScript and call JavaScript functions from Elixir.
- `Popcorn.Proxy`: connect JavaScript calls and casts to your application's GenServers.
- `Popcorn.Fetch`: send HTTP requests through the browser, directly or with Req.

## Installation

Add the Elixir package to your Mix dependencies:

```elixir
defp deps do
  [{:popcorn, "0.4.0-next.0"}]
end
```

Install the matching JavaScript package with `npm install @swmansion/popcorn@next`.
The npm package includes both `core` and `crypto` runtime variants.
The bundler plugin selects one from your application's dependencies.
Use `runtimeVariant` to override the selection. Both variants use this Hex package.

Compile your application with `mix deps.get` and `mix compile` before building its JavaScript assets.
The bundler plugin invokes Mix to package your application and its standard-library dependencies.
Use the toolchain in [popcorn/mise.toml](https://github.com/software-mansion/popcorn/blob/v0.4.0-next.0/popcorn/mise.toml) for this release.

See the [JavaScript setup guide](https://github.com/software-mansion/popcorn/blob/v0.4.0-next.0/popcorn/js/README.md) for bundler configuration and production headers.
See the [versioned API documentation](https://popcorn.hexdocs.pm/0.4.0-next.0/) for the Elixir API.
