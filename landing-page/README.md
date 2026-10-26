# Popcorn Landing Page

Built with [Astro](https://astro.build).

## Usage

Build both Wasm runtime variants and the JavaScript package before starting the
site for the first time. Pong uses LocalLiveView, which requires the crypto
variant. Run these commands from the repository root with Emscripten available:

```bash
pnpm install
mise run build-otp debug
mise run build-otp-with-crypto debug
pnpm -F ./popcorn/js build
```

From the repository root:

```bash
mise run dev --project landing-page
```

or directly from this directory:

```bash
pnpm run dev
```

| Command            | Action                                       |
| :----------------- | :------------------------------------------- |
| `pnpm run dev`     | Start local dev server at `localhost:4321`    |
| `pnpm run build`   | Build production site to `./dist/`            |
| `pnpm run preview` | Preview the build locally, before deploying   |
