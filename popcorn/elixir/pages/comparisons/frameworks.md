# Popcorn and Elixir web frameworks

Popcorn is a browser runtime and JavaScript bridge. It does not define pages,
components, templates, routing, or application state.

## Choose the correct layer

| Tool             | Main role                             | Where application state runs       |
| ---------------- | ------------------------------------- | ---------------------------------- |
| Popcorn          | Run BEAM applications in the browser  | Browser BEAM processes             |
| Phoenix LiveView | Build server-driven interactive pages | Server BEAM processes              |
| LocalLiveView    | Run Phoenix LiveView in the browser   | Browser virtual machine            |
| Hologram         | Build full-stack Elixir interfaces    | Browser JavaScript and server code |

## Popcorn and Phoenix LiveView

Phoenix LiveView keeps its process on the server. The browser sends events and
applies server-rendered diffs.

Popcorn runs a separate BEAM virtual machine in the browser. It can perform
local work without a server round trip.

Use LiveView for server-owned interactive pages. Add Popcorn when a feature
needs local OTP processes, offline work, or direct browser computation.

## Popcorn and Hologram

Hologram is a full-stack user interface framework. It compiles parts of Elixir
code to JavaScript and provides pages, components, routing, state, and
client-server commands.

Popcorn packages BEAM bytecode and runs it on a BEAM virtual machine. It lets
the application choose its JavaScript user interface.

Choose Hologram when you want its complete interface model. Choose Popcorn when
the browser needs OTP semantics or must run an existing BEAM application layer.

## Popcorn and LocalLiveView

LocalLiveView is a library built on Popcorn. It extends Phoenix LiveView to run
state in the browser. See the [LocalLiveView documentation](https://local-live-view.hexdocs.pm/welcome.html).

The current LocalLiveView package uses Popcorn 0.3. Do not assume that it
supports the Popcorn 0.4 OTP runtime.
