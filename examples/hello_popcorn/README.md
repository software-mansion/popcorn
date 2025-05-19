# Hello Popcorn 👋

This is a very simple example of using Popcorn: it displays `Hello from WASM!` on the homepage and `Hello console!` in the browser console (see `lib/hello_popcorn.ex`).

Usage:

```bash
mix deps.get
mix popcorn.cook
elixir server.exs
```

and visit [localhost:4000](http://localhost:4000)

This example also contains [Playwright](https://github.com/mechanical-orchard/playwright-elixir)-based tests. To run them, type:

```bash
mix playwright.install
mix test
```
