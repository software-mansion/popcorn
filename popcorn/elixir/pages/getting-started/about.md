# What is Popcorn

Popcorn is a library that runs Elixir and Erlang applications in a web browser.
It uses the Erlang/OTP VM compiled to WebAssembly, allowing you to run your application fully on user's device.

It's a perfect choice for code playgrounds, interactive documentation, offline tools, and more.

## What's in the package

Popcorn provides:

- an Erlang/OTP VM adapted to work in browsers.
- [APIs](js-apis.md) to communicate with JavaScript from Elixir (and vice versa).
- Scripts to package application files, HTML, and the VM for deployment.

It also provides browser-specific alternatives for common tasks. For example, `Popcorn.Fetch` helps with sending HTTP requests through the browser. It can be used directly or as [Req](https://hex.pm/packages/req) adapter.

## Limitations

The VM supports the same functionality as a native one. You can use processes, supervision trees, OTP components (e.g. `GenServer`), timers, monitors, ETS, ...

Browsers do have their limits though. We disabled distribution, TCP/UDP sockets, and other features that browsers can't support easily. See ["Compatibility and browser limits"](compatibility.md) to read more.

## Where to start

See ["Installation"](installation.md) to add Popcorn to a project.

To build a bare bones application, see ["Hello World"](hello-world.md). More realistic project and advanced API use are shown in ["Hello counters"](hello-counters.md) document.
