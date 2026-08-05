# First steps

## Usage
To use LocalLiveView you need to:
1. Add `:local_live_view` to your mix.exs file:
```elixir
defmodule LocalThermostat.MixProject do
  use Mix.Project
  ...
  defp deps do
    [
      {:local_live_view, "~> 0.1"}
    ]
  end
end
```
2. Run `mix llv.install`. It wires up the JS side for you: the bundle ships in
   the package, so there is no npm step — Phoenix's esbuild resolves
   `import { LLVEngine } from "local_live_view"` through `deps/`.

   > Taking LocalLiveView from git instead (`github: "software-mansion/popcorn",
   > sparse: "local-live-view"`) also works, but then the JS bundle is not
   > prebuilt and `mix llv.build` needs Node.js + pnpm to build it from source.
3. Define your LocalLiveView in the `lib` directory:
```elixir
defmodule ThermostatLive do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>Current temperature: {@temperature}°F</p>
    <button pop-click="inc_temperature">+</button>
    <p>Country: {@country}</p>
    """
  end

  def mount(_params, _session, socket) do
    temperature = 65
    {:ok, assign(socket, :temperature, temperature)}
  end

  def handle_event("inc_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 + 1))}
  end
end
```
4. Add html tag to render defined view, by using data-pop-view:
```html
<div data-pop-view="ThermostatLive"></div>
```

## Build

To build the project after defining the above, run:

```bash
mix deps.get
mix build
```

This will generate the necessary scripts and popcorn files into the output directory.

## Serve

To serve the project locally:

```bash
mix dev
```

and visit [localhost:4000](http://localhost:4000).

You can also use `mix popcorn.server` directly, or any HTTP server that sets the required headers:

```
Cross-Origin-Opener-Policy: "same-origin"
Cross-Origin-Embedder-Policy: "require-corp"
```

## Examples

- `examples/local-lv-thermostat` - Simple thermostat app demonstrating basic LocalLiveView state management and events.
- `examples/local-lv-forms` - Form handling with LocalLiveView, including validation and submission.
- `examples/local-lv-compare` - Side-by-side comparison of Phoenix LiveView and LocalLiveView.
