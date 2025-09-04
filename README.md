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

The examples are hosted at [popcorn.swmansion.com](https://popcorn.swmansion.com), and the source code is in the `examples` directory.

## Development

We use [`mise`](https://mise.jdx.dev) to manage dependencies. [Install it](https://mise.jdx.dev/installing-mise.html) and install deps and dev tools with:

```shell
mise install
```

Then, you should setup pre-commit hooks using [lefthook](https://lefthook.dev):

```shell
lefthook install
```

### Testing

Popcorn tests can be run either on WASM via Playwright or natively on UNIX. To run them on WASM, run

```shell
TARGET=wasm mix test
```

To run tests on UNIX, use

```shell
mix popcorn.build_runtime --target unix
```

to build AtomVM from source. Make sure you have [AtomVM dependencies](https://github.com/atomvm/atomvm?tab=readme-ov-file#dependencies) installed. Then, run

```shell
mix test
```

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

Licensed under the [Apache License, Version 2.0](LICENSE)
