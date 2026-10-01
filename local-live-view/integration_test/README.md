# LocalLiveView integration tests

A Phoenix app exercising LocalLiveView end to end in a browser, with a
Playwright suite. It isn't part of the `local_live_view` package.

## Running

```sh
mix deps.get
mix test
```

`mix test` rebuilds the Wasm bundle and `app.js` against the current
`local_live_view` sources, then `test/e2e_test.exs` serves the app on :4904
from the test VM and runs the Playwright suite in `test/playwright`. It needs
pnpm, and the workspace dependencies installed (`pnpm install` at the repo
root).

To run the suite against a server started by hand, e.g. to debug a test:

```sh
mix setup
PORT=4905 mix phx.server
cd test/playwright && BASE_URL=http://localhost:4905 pnpm test
```

## What's covered

- `ssr.spec.js`: server-side rendering. Views are on the page, inert, before
  the Wasm runtime boots; `llv_ssr={false}`; the main view rendered with
  `handle_params/3`; hosted and mirrored views; the local view taking over the
  server-rendered element; views added by a live navigation; user assigns
  named like LocalLiveView's own settings.
- `hosted.spec.js`: a view inside a host LiveView. Assigns from the host,
  `push_server_event` with optimistic edits confirmed or rolled back,
  `handle_push_error`, forms, `phx-drag*` and `phx-mouse*` bindings, updates
  across clients, crash recovery, live navigation away and back, mirrors.
- `main_navigation.spec.js`: pages whose main view is local, with and without
  a regular LiveView in the layout. `handle_params/3` at mount and on patches,
  `push_patch`, patch links, back and forward, other views patching, the full
  URL, a patch while the main view mounts, `push_navigate`, forms; and a page
  nothing owns.
- `hosted_navigation.spec.js`: pages with a host LiveView. `push_patch` from
  views inside and outside the host, `push_navigate` from `handle_event/3` and
  `handle_info/2`, live navigation between pages rendering the same view.

## The app

- `local/lib`: the local views, compiled to the Wasm bundle and, for
  server-side rendering, into the app.
- `lib/llv_integration_web/router.ex`: the pages. `live_local` routes for
  pages whose main view is local, LiveView routes hosting local views, and
  plain controller pages.
- `lib/llv_integration_web/layouts.ex`: the root layout can also render a
  local view (`Patcher`) or a regular LiveView (`LayoutLive`) outside the
  page's own views.
