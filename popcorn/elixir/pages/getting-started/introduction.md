# What is Popcorn?

Popcorn runs Elixir and Erlang applications in a web browser.

It uses the Erlang/OTP VM, compiled to WebAssembly: you can run your application on user's device.

It's a perfect choice for code playgrounds, interactive documentation, offline tools, and more.

## What Popcorn provides

Popcorn provides:

- a VM compiled for browsers use-case,
- [APIs](TODO) to communicate with JavaScript (and with Elixir from JavaScript),
- [a packager](TODO) to create files for deployment.

It also provides browser-specific alternatives for common tasks. For example, `Popcorn.Fetch` Req adapter sends HTTP requests through the browser.

The VM supports same functionality as native one, with small exceptions (such as distribution or TCP sockets). You can use processes, supervision trees, OTP components (e.g. `GenServer`), timers, monitors, ...

See [Compatibility and browser limits](compatibility.html) before you migrate
an existing application.

## How to start

Use [Installation](installation.html) to add Popcorn to a project. Then complete
[Build your first Popcorn application](first-application.html).
