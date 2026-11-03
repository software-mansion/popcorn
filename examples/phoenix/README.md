# Phoenix in Popcorn (spike)

An unmodified Phoenix and LiveView app running inside Popcorn, without a network.
Requests don't go through TCP or an HTTP server: JavaScript sends them to the VM
as maps, and the VM serves them with `Endpoint.call/2`, like `Phoenix.ConnTest`
does. LiveView connects through Phoenix's own WebSocket transport.

## Running

```sh
mix deps.get
mix popcorn.dev
```

Open [localhost:4000](http://localhost:4000). The page runs its checks and lists
them; `node scripts/check.mjs` runs them in headless Chromium.

## How it works

- `Popcorn.Plug` takes requests from `popcorn.call("popcorn_plug", {method, url, headers, body})`.
  Each request gets its own process, which calls the endpoint with a conn of
  Popcorn's adapter.
- WebSocket requests go through `Phoenix.Transports.WebSocket` (origin check,
  session from the cookie, CSRF token), which upgrades them. The request process
  then becomes the socket and drives its WebSock callbacks, like Bandit does.
  `WebSockAdapter` upgrades the conns of servers other than the ones it knows
  from version 0.7.1, which `mix.exs` requires.
- On the page, `LiveSocket` gets a WebSocket-like `transport` that sends frames
  to that process with `popcorn.send` and receives them with `onEvent`.
- Responses built in JavaScript can't set cookies, so the page keeps a cookie jar.

## Shortcuts

- The page runs the app inline. A real setup would run it in an iframe whose
  requests a Service Worker sends to the VM in the parent page.
- LiveView only joins when the page's URL routes to the view, so the page moves
  to `/counter` with `history.replaceState` before connecting.
