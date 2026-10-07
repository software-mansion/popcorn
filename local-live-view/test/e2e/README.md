# End-to-end tests

A Phoenix app exercising LocalLiveView in a browser, with a Playwright suite.
`mix test` runs it, as part of local_live_view's tests: `e2e_test.exs` builds
what the app's pages load, serves the app from the test VM on :4904 and runs
the suite in `playwright/`. It needs pnpm, with the workspace dependencies
installed (`pnpm install` at the repo root).

```sh
mix test                       # everything
mix test test/e2e/e2e_test.exs # just this suite
mix test --exclude e2e         # skip it
```

To serve the app by hand, e.g. to debug a test:

```sh
MIX_ENV=test mix run --no-halt test/e2e/server.exs
cd test/e2e/playwright && BASE_URL=http://localhost:4905 pnpm test
```

## The app

- `support/`: the server side. `LocalLiveView.E2E.Server` builds the assets
  and starts the endpoint. `router.ex` has the pages: `live_local` routes for
  pages whose main view is local, LiveView routes hosting local views, and
  plain controller pages. The root layout (`layouts.ex`) can also render a
  local view (`Patcher`), a patch link, or a regular LiveView (`LayoutLive`)
  outside the page's own views.
- `local/`: the local views. They're compiled into local_live_view's test
  build, for server-side rendering, and built to the Wasm bundle by the
  `local/mix.exs` project.
- `assets/app.js`: the app's JS, bundled with esbuild.

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
