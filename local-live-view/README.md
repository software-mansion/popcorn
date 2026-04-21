# LocalLiveView

**LocalLiveView is a library for running Phoenix LiveView state in the browser, powered by [Popcorn](https://hexdocs.pm/popcorn).**

LocalLiveView compiles your Elixir modules to WebAssembly and runs them via Popcorn directly in the browser. You get zero-latency UI and offline capability while using the same Phoenix LiveView API you already know.

## Documentation

The API documentation and guides are available at <https://hexdocs.pm/local_live_view>

## Examples

The source code for all examples is in the `examples/` directory of this repository:

- `local-lv-thermostat` — Basic state management and events
- `local-lv-forms` — Form handling with validation and Mirror Sync
- `local-lv-compare` — Side-by-side comparison of Phoenix LiveView vs LocalLiveView
