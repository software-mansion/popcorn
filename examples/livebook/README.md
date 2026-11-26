# Livebook in Popcorn (spike)

[Livebook](https://livebook.dev) 0.19, unmodified from Hex, running in the
browser's VM. Each notebook runs in a VM of its own, which installs packages
from Hex with `Mix.install/2`. The VMs are Erlang nodes that the page connects.
Nothing goes over a network but the packages' downloads: requests and WebSocket
frames go between the page and Livebook's VM.

## Running

It needs Popcorn's runtime built from this branch, with distribution
(`scripts/build-beam.sh --with-crypto release`, then `pnpm build` in `popcorn/js`).

```sh
mix deps.get
mix popcorn.dev
```

`mix popcorn.dev` also fetches the dependencies of `notebook_vm/`, the package
of the notebooks' VMs, and cooks it, but only recooks Livebook's on changes.

Open [localhost:4000](http://localhost:4000).

`elixir serve.exs` hosts what `mix popcorn.dev` cooked, as a static host would,
at [localhost:4000](http://localhost:4000) too, or at another port, as with
`elixir serve.exs 4001`. Unlike `mix popcorn.dev`, it serves the page for
Livebook's links too, like `/import?url=…`.

`mix cook` cooks `public/` as `mix popcorn.dev` does, without serving it. The
demo deployment, `docker/livebook.Dockerfile`, serves it with nginx, configured
like `serve.exs` in `docker/livebook.nginx.conf`.

`pnpm test` in `test/playwright` hosts it the same way, then opens Livebook in
headless Chromium, creates notebooks and evaluates cells in them. Set
`LIVEBOOK_URL` to test another server. The suite is a member of the repo's pnpm
workspace, so it needs `pnpm install` first.

`mix test` tests `LivebookWasm.Plug` on the host.

## How it works

- `public/index.html` is a shell page that owns the VMs, so they outlive
  Livebook's navigations. Livebook runs in an iframe, under a path of the tab's
  own, `/tab/<id>/`: `LivebookWasm.Application` sets it as Livebook's base path.
- `public/sw.js`, a Service Worker, sends the iframe's navigations, form posts
  and fetches to the shell, which calls `LivebookWasm.Plug` in Livebook's VM.
  Livebook's static files load from the network: `mix popcorn.dev` copies them
  to `public/`, and they're left out of the VM's package.
- Each tab registers the Service Worker for its own path, as a worker that
  tabs shared couldn't tell which tab a navigation is for. The worker sends the
  requests through a hidden relay frame under the path, as the shell isn't.
  Each tab holds a lock for its path, and the shell removes the workers of
  closed tabs.
- Only the shell's iframe reaches the tab's VM, so the Service Worker
  redirects a page that opens in a tab of its own, like a link opened in a new
  tab, to a new shell, with a Livebook of its own, on its home page.
- Service Workers can't intercept WebSockets, so the Service Worker adds
  `public/popcorn-frame.js` to Livebook's pages, which replaces `WebSocket` with
  one whose other end is a socket process in the VM. The script also passes the
  requests on in the relay frame.
- Responses built in JavaScript can't set cookies, so the shell keeps them.
- `LivebookWasm.Plug` serves the requests with `LivebookWeb.Endpoint`, as a
  web server would, but with no network: each request and WebSocket runs in a
  process of its own, and their responses and frames go to the shell.
- `LivebookWasm.Application` starts Livebook after its stand-ins, and
  `LivebookWasm.Plug` after Livebook: memsup, which Livebook calls for memory
  usage, doesn't run in the browser.
- The home page lists the example notebooks of `priv/notebooks/`, which
  Popcorn packages: `LivebookWasm.Application` copies them to the VM's home
  directory and stars them. Livebook's own notebooks, under Learn, are part of
  its code.
- `config/config.exs` has the settings Livebook's config files and
  `Livebook.config_runtime/0` would make, as Popcorn evaluates only this file.
- Outputs' iframes, like Kino's JavaScript outputs', load from Livebook's
  origin, under the tab's path, so the Service Worker sends their requests to
  the VM.

### Notebooks' VMs

- `LivebookWasm.Runtime` is the notebooks' runtime. When Livebook connects it,
  the shell boots a VM as a node with its own name. Disconnecting the runtime
  shuts the VM down.
- The VMs boot the package of `notebook_vm/`, a project of its own, which
  `mix popcorn.dev` cooks into `public/notebook_vm/`. It has Mix and Hex but
  none of Livebook's code or dependencies: Livebook loads its runtime's code
  into the VM, as into the standalone runtime's OS process, and notebooks can
  install any packages.
- Popcorn's package leaves out Elixir's and Erlang's docs, which Livebook's
  code completion and docs need, so `mix popcorn.dev` packs the host's docs of
  the package's modules in `public/notebook_vm/docs.tar`, with a gzipped copy
  for the host to serve. The VMs boot without them, and `NotebookVM.Docs`
  loads them after. Specs and types aren't there:
  they'd need the modules' debug info. Neither are the docs of the VM's
  preloaded modules, like `:erlang`.
- The shell relays the nodes' distribution, also between tabs: see
  `public/distribution.js`. The tabs register their nodes with a SharedWorker,
  which gives two tabs a `MessageChannel` of their own for each connection, so
  that other code of the origin can't listen in. A tab disconnects another's
  nodes once its lock is free, when it closes. Nodes' names are random, to
  differ across tabs, and each tab's nodes have a random cookie of their own.
  So a notebook can connect to a node in another tab with the cookie from
  there, as with `Node.set_cookie(:"runtime_…@popcorn", cookie)`.
- The VMs' host differs from Livebook's, so that Livebook sends files rather
  than paths to them: each VM has its own file system.

### Mix.install

- Hex downloads packages with `:httpc`, which Popcorn makes request with the
  browser's `fetch()`, from jsDelivr's mirror of hex.pm: unlike hex.pm, it lets
  other origins fetch. Hex checks the packages, so a mirror can't alter them.
- Mix compiles rebar3 packages by running rebar3, so `NotebookVM.MixShell`
  compiles them instead, with `NotebookVM.Rebar`: their Erlang sources, Leex
  and Yecc files, and `.app.src` files.
- Hex is a Mix archive, so `notebook_vm/`'s `mix popcorn.cook` copies it among
  the build's applications for Popcorn to package it.
- The notebooks' VMs have OTP's headers, which Popcorn packages, for the Erlang
  code that includes them, like Mint's.
- Req requests with the browser's `fetch()` too: Popcorn installs its Req
  adapter once `Mix.install/2` has started Req.

### Postgres in the page

- The notebooks can use Postgres: PGlite, Postgres compiled to WebAssembly,
  which the shell loads from jsDelivr on the first connection. The tab has one
  database, in memory, which its notebooks share.
- The VMs have no sockets, so Postgrex connects with gen_tcp's
  `{tcp_module, popcorn_tcp}` option, `socket_options: [tcp_module:
  :popcorn_tcp]`, to the host `pglite`. Popcorn's `popcorn_tcp` passes the
  connection's data to its `tcp` option, which the shell sets to
  `public/pglite.js`. That passes Postgres' wire protocol to PGlite.
- PGlite has a single session, which the connections share. A connection has it
  from its first message until Postgres is ready for a query out of a
  transaction. So a connection that waits for another while in a transaction
  waits forever, as Ecto's migration lock and Postgrex's type server would with
  a bigger pool: repos use `pool_size: 1`, and migrations `migration_lock:
  false`. Named prepared statements are shared too, so repos use `prepare:
  :unnamed`.
- PGlite doesn't send notifications to other connections, so Oban uses its
  `Oban.Notifiers.PG` notifier: see `priv/notebooks/oban.livemd`.

### Opening notebooks from links

- Livebook opens a notebook from a link to `/import?url=<notebook>`. That's
  where livebook.dev's "Run in Livebook" badges send people, with the Livebook
  address they set on livebook.dev, which can be this example's.
- livebook.dev checks the address with a request for `/public/health`, from
  its own pages, which Livebook answers with `{"application": "livebook"}`
  and a CORS header. The shell's host answers it the same way, as `serve.exs`
  does: the request doesn't reach the Service Worker, which only gets the
  requests of the example's own pages.
- The shell opens a link like that in its iframe, under the tab's path. Its
  host has to serve the shell's page for the link's path, as `serve.exs` does.
  Once Livebook has opened the link, the shell replaces its own URL with `/`,
  so that reloading doesn't open the link again.
- Livebook downloads the notebook with Req, which Popcorn makes request with
  `fetch()`. So the notebook's server has to let other origins fetch, as
  GitHub's does.

## What doesn't work

- Isolating outputs' JavaScript: Livebook serves their iframes from a separate
  origin, which the Service Worker couldn't send to the VM.
- Packages that build with Make or have native code, and rebar3's hooks,
  plugins and profiles.
- Persistence: Livebook's and the notebooks' files are in the VMs' memory, so a
  reload loses them, including the installed packages.
- The runtime settings (Configure), which know only Livebook's own runtimes.
- Evaluating right after typing can evaluate the cell's previous source; the
  tests wait half a second after typing.
