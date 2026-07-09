# Kanban e2e tests (Playwright)

End-to-end tests for the collaborative kanban: board create/list, add/remove
columns and tasks, drag & drop (reorder + cross-column), realtime sync across two
clients, optimistic rollback, and persistence.

This is a `@playwright/test` suite and a member of the repo's pnpm workspace.

## How it runs

Unlike a typical Playwright setup, this config does **not** start a web server. The
suite is wrapped in an ExUnit test (`test/e2e_test.exs`, tagged `:e2e`) that owns the
server: it starts the Phoenix endpoint **in the same BEAM that runs the test**
(MIX_ENV=test) on :4901, points the SQL sandbox at shared mode so the browser's
requests share the test's DB connection, runs this Playwright suite, then tears the
server down. Everything the suite writes rolls back when the test finishes.

## Run

From Elixir (recommended) — runs as part of `mix test`, from the app root:

```sh
mix test               # builds assets, then runs unit + e2e tests
mix test --exclude e2e # fast: unit tests only (skip this suite)
mix test --only e2e    # just this suite
```

`mix test` prepends `mix llv.build`, so the WASM bundle the browser boots from is
current (adds ~30–60s; use `--exclude e2e` while iterating on unit tests). On a clean
checkout the e2e test also builds `app.js`/`app.css` once via `mix assets.build`.

Directly with pnpm/Playwright (from the repo root for the workspace install):

```sh
pnpm install                              # at the repo root (installs this package)
cd examples/local-lv-kanban/test/playwright
pnpm test                                 # requires a server already on :4901
pnpm run test:headed                      # watch it drive a real browser
```

The direct `pnpm test` path no longer starts a server — bring one up yourself first,
e.g. `mix phx.server` (or `iex -S mix phx.server`) in the app root, listening on :4901.
Tests run serially against one server + DB.

## Notes

- Drag & drop uses native HTML5 drag events (see `tests/helpers.js`); Playwright's
  mouse-based drag does not trigger the kanban's `phx-drag*` bindings, so the
  helpers dispatch real `DragEvent`s.
- Each test creates its own board, so runs don't interfere.
