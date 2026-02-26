[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/dark-mode-logo-text.svg">
  <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/light-mode-logo-text.svg">
  <img alt="Popcorn" src="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/fallback-logo-text.svg">
</picture>

**Popcorn is a library that allows you to run client-side Elixir in browsers, with JavaScript interoperability**

Popcorn is early stages and may break. Please report an issue if it does. Contributions are very welcome, but please open an issue before committing too much effort.

Under the hood, Popcorn runs [AtomVM](https://github.com/atomvm/AtomVM), a tiny Erlang VM.

## Documentation

The API documentation and guides are available at <https://hexdocs.pm/popcorn>

## Examples

The examples are hosted at [popcorn.swmansion.com](https://popcorn.swmansion.com), and the source code is in the `examples/` directory.

## Repository Structure

```
popcorn/
├── popcorn/
│   ├── elixir/      # Elixir library
│   └── js/          # JS library
├── examples/
├── landing-page/
├── language-tour/
├── local-live-view/
├── scripts/
└── docker/
```

## Development

We use [`mise`](https://mise.jdx.dev) to manage tool versions and run tasks. [Install it](https://mise.jdx.dev/installing-mise.html), then:

```shell
mise install
mise run dev
```

This installs all dependencies (Elixir, Node, pnpm) and starts the JS library in watch mode.

To develop with an example or project:

```shell
mise run dev --example hello-popcorn
mise run dev --project landing-page
mise run dev --project language-tour
```

Run `scripts/dev.sh --help` to see all available examples and projects.

### Testing

```shell
mise run test               # Elixir unix tests (default)
mise run test --wasm     # Elixir wasm tests
mise run test --js       # JS tests
```

AtomVM is built automatically if artifacts are missing. Make sure you have [AtomVM dependencies](https://github.com/atomvm/atomvm?tab=readme-ov-file#dependencies) installed.

### Other tasks

```shell
mise run clean              # Clean build artifacts
mise run clean --all     # Clean everything including examples
```

All tasks are thin wrappers around `scripts/*.sh` — you can run those directly.

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

Licensed under the [Apache License, Version 2.0](LICENSE)
