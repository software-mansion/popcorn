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

Your application runs on the user's device, with OTP processes, message passing,
and supervision. JavaScript and Elixir communicate through Popcorn's APIs.

## Documentation

- [Popcorn 1.0 documentation](https://hexdocs.pm/popcorn/): installation, tutorials, guides, and API reference.
- [JavaScript package](popcorn/js/README.md): bundler setup and JavaScript usage.
- [Elixir package](popcorn/elixir/README.md): application setup and packaging.
- [LocalLiveView](local-live-view/README.md): Phoenix LiveView running in the browser.

Popcorn 1.0 is a prerelease based on Erlang/OTP. The stable npm channel still
uses Popcorn 0.3 and AtomVM; see the [0.3 documentation](https://hexdocs.pm/popcorn).

## Examples

Try the demos at [popcorn.swmansion.com](https://popcorn.swmansion.com/#examples)
and browse their [source code](examples/). The
[Elixir language tour](https://elixir-language-tour.swmansion.com/) also runs on Popcorn.

Community projects include [Popcorn on iOS](https://github.com/u9g/uno-royale)
and [LocalLiveView demos](https://petermm.github.io/popcorn_live_view/).

## Development

We use [mise](https://mise.jdx.dev) to manage tools and tasks. See
[mise.toml](mise.toml) for tool versions and available tasks, and
[scripts/](scripts/) for development, testing, and build scripts.
The [Hello Popcorn example](examples/hello-popcorn/README.md) includes local build and run instructions.

Please report [issues](https://github.com/software-mansion/popcorn/issues) and
discuss large contributions before you start work.

## Authors

Popcorn is created by Software Mansion.

Since 2012 [Software Mansion](https://swmansion.com/) is a software agency with experience in building web and mobile apps as well as complex multimedia solutions. We are Core React Native Contributors and experts in live streaming and broadcasting technologies. We can help you build your next dream product – [Hire us](https://swmansion.com/contact/projects).

Copyright 2025, [Software Mansion](https://swmansion.com/)

[![Software Mansion](https://logo.swmansion.com/logo?color=white&variant=desktop&width=200&tag=membrane-github)](https://swmansion.com/)

## License

Licensed under the [Apache License, Version 2.0](LICENSE)
