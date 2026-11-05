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
at [localhost:4001](http://localhost:4001). Unlike `mix popcorn.dev`, it serves
the page for Livebook's links too, like `/import?url=…`.

`node scripts/check.mjs` hosts it the same way, then opens Livebook in headless
Chromium, creates notebooks and evaluates cells in them. Pass it a URL to check
another server.

## How it works

- `public/index.html` is a shell page that owns the VMs, so they outlive
  Livebook's navigations. Livebook runs in an iframe, under a path of the tab's
  own, `/tab/<id>/`: `LivebookWasm.Application` sets it as Livebook's base path.
- `public/sw.js`, a Service Worker, sends the iframe's navigations, form posts
  and fetches to the shell, which calls `Popcorn.Plug` in Livebook's VM.
  Livebook's static files load from the network: `mix popcorn.dev` copies them
  to `public/`, and they're left out of the VM's package.
- Each tab registers the Service Worker for its own path, as a worker that
  tabs shared couldn't tell which tab a navigation is for. The worker sends the
  requests through a hidden relay frame under the path, as the shell isn't.
  Each tab holds a lock for its path, and the shell removes the workers of
  closed tabs.
- Service Workers can't intercept WebSockets, so the Service Worker adds
  `public/popcorn-frame.js` to Livebook's pages, which replaces `WebSocket` with
  one whose other end is a socket process in the VM. The script also passes the
  requests on in the relay frame.
- Responses built in JavaScript can't set cookies, so the shell keeps them.
- `Popcorn.Plug` serves the requests with `LivebookWeb.Endpoint`, like the
  Phoenix example.
- `LivebookWasm.Application` starts Livebook after its stand-ins, and
  `Popcorn.Plug` after Livebook: memsup, which Livebook calls for memory usage,
  doesn't run in the browser.
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
- The shell relays the nodes' distribution with Popcorn's
  `createDistributionRelay()`.
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
- The notebooks' VMs have OTP's headers, with Popcorn's `include_headers`, for
  the Erlang code that includes them, like Mint's.
- Req requests with the browser's `fetch()` too: Popcorn installs its Req
  adapter once `Mix.install/2` has started Req.

### Opening notebooks from links

- Livebook opens a notebook from a link to `/import?url=<notebook>`. That's
  where livebook.dev's "Run in Livebook" badges send people, with the Livebook
  address they set on livebook.dev, which can be this example's.
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
  check waits half a second after typing.
