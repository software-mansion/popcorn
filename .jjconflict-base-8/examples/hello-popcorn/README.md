# Hello Popcorn 👋

This is a very simple example of using Popcorn with the OTP/BEAM WebAssembly
runtime: it displays `Hello from Wasm!` on the homepage and `Hello console!` in
the browser console (see `lib/hello_popcorn.ex`).

Install the JavaScript dependencies and build the OTP package from the
repository root:

```bash
pnpm install
mise run build-otp-js
```

Then run the example directly from its directory:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000).

This example also contains [Playwright](https://playwright.dev)-based tests. To
run them from the `assets` directory, type:

```bash
pnpm exec playwright install chromium
pnpm test
```
