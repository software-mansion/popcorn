# What is Popcorn?

Popcorn runs Elixir and Erlang applications in a web browser.

It uses the BEAM virtual machine from Erlang/OTP, compiled to WebAssembly.
Your compiled BEAM code runs in a Web Worker on the user's device.

## What Popcorn provides

Popcorn provides a runtime and a bridge.

The runtime supports standard OTP concepts such as processes, GenServers,
supervisors, monitors, and message passing. The bridge connects those processes
to JavaScript in the page.

Use Popcorn when an application needs local work in the browser. Examples
include offline tools, simulations, interactive documentation, and code
playgrounds.

Popcorn does not provide a user interface model. Use plain JavaScript, React,
Phoenix, or another interface with it.

## How Popcorn differs from server Elixir

A Popcorn application runs inside the browser sandbox. It does not have direct
access to operating-system processes, native sockets, or arbitrary dynamic
native implemented functions (NIFs).

Popcorn provides browser-specific alternatives for common tasks. For example,
`Popcorn.Fetch` sends HTTP requests through the browser.

See [Compatibility and browser limits](compatibility.html) before you migrate
an existing application.

## How to start

Use [Installation](installation.html) to add Popcorn to a project. Then complete
[Build your first Popcorn application](first-application.html).
