[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/dark-mode-logo-text.svg">
  <source media="(prefers-color-scheme: light)" srcset="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/light-mode-logo-text.svg">
  <img alt="Popcorn" src="https://raw.githubusercontent.com/software-mansion/popcorn/refs/heads/main/assets/fallback-logo-text.svg">
</picture>

[![Ad](https://swm-delivery.com/www/images/zone-gh-popcorn-1?n=1)](https://swm-delivery.com/www/delivery/ck-slug.php?zoneid=zone-gh-popcorn-1&n=1)
[![Ad](https://swm-delivery.com/www/images/zone-gh-popcorn-2?n=1)](https://swm-delivery.com/www/delivery/ck-slug.php?zoneid=zone-gh-popcorn-2&n=1)
[![Ad](https://swm-delivery.com/www/images/zone-gh-popcorn-3?n=1)](https://swm-delivery.com/www/delivery/ck-slug.php?zoneid=zone-gh-popcorn-3&n=1)

**Popcorn runs Elixir and Erlang applications in the browser on a BEAM virtual machine compiled to WebAssembly.**

Please report issues and discuss large contributions before you start work.

### Release channels

The stable 0.3 release uses [AtomVM](https://github.com/atomvm/AtomVM).

The 0.4 prerelease uses the BEAM virtual machine from Erlang/OTP, compiled to
WebAssembly. It runs compiled BEAM code in a Web Worker and supports standard
OTP processes, message passing, and supervision.

Use the [Popcorn 0.4 setup guide](popcorn/js/README.md) for the prerelease. The
stable npm channel remains on Popcorn 0.3 until the 0.4 release.

## Documentation

The stable documentation is available at <https://hexdocs.pm/popcorn>.

The versioned 0.4 prerelease documentation is available at
<https://popcorn.hexdocs.pm/0.4.0-next.0/>.

## Examples

The examples are hosted at [popcorn.swmansion.com](https://popcorn.swmansion.com), and the source code is in the `examples/` directory.

See also third-party examples:

- Running Popcorn on iOS (WebView based): https://github.com/u9g/uno-royale
- A collection of local Live View demos: https://petermm.github.io/popcorn_live_view/

## Repository Structure

- **`popcorn-2/`** - Popcorn 0.3 implementation based on AtomVM.
- **`popcorn/`** - Popcorn 0.4 implementation based on Erlang/OTP.
- **`examples/`** - Example projects showcasing Popcorn features, hosted at [popcorn.swmansion.com](https://popcorn.swmansion.com/#examples). Examples use development version of Popcorn.
- **`landing-page/`** - Popcorn [landing page](https://popcorn.swmansion.com/).
- **`language-tour/`** - Interactive [Elixir language tour](https://elixir-language-tour.swmansion.com/) running purely in the browser.
- **`local-live-view/`** - Experimental client-side LiveView implementation.
- **`scripts/`** - Shell scripts for development, testing, and CI tasks.
- **`docker/`** - Dockerfiles and nginx configs for CI and deployment.

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
mise run test            # Elixir unix tests (default)
mise run test --wasm     # Elixir wasm tests
mise run test --js       # JS tests
```

The required runtime build depends on the package that you change. See the
package documentation for its toolchain and native build requirements.

### Other tasks

```shell
mise run clean           # Clean build artifacts
mise run clean --all     # Clean everything including examples
```

All tasks are thin wrappers around `scripts/*.sh` — you can run those directly.

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

Licensed under the [Apache License, Version 2.0](LICENSE)
