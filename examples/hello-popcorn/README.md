# Hello Popcorn 👋

This is a very simple example of using Popcorn: it displays `Hello from WASM!` on the homepage and `Hello console!` in the browser console (see `lib/hello_popcorn.ex`).

Usage from the repository root:

```bash
pnpm install
mise run dev --example hello-popcorn
```

or directly from the example directory:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000)

This example also contains [Playwright](https://github.com/mechanical-orchard/playwright-elixir)-based tests. To run them, type:

```bash
mix playwright.install
mix test
```
