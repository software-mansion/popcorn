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
      {:local_live_view, github: "software-mansion/popcorn", sparse: "local-live-view"}
    ]
  end
end
```
2. Attach local_live_view.js script in your .html file:
```html
<html>
    <script type="module" src="./local_live_view/local_live_view.js" defer></script>
    <body>
    </body>
</html>
```
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
<html>
    <script type="module" src="./local_live_view/local_live_view.js" defer></script>
    <body>
    <div data-pop-view="ThermostatLive"></div>
    </body>
</html>
```

## Local components

When you need to seed a local view with state from the server, use a
`LocalComponent` instead. It is the browser-side counterpart of
`Phoenix.LiveComponent`: same `mount/1`, `update/2`, `render/1` and
`handle_event/3` callbacks, and it can be handed assigns when mounted.

```elixir
defmodule Cart do
  use LocalComponent

  @impl true
  def mount(socket), do: {:ok, assign(socket, open: false)}

  @impl true
  def update(%{items: items}, socket), do: {:ok, assign(socket, :items, items)}

  @impl true
  def render(assigns) do
    ~H"""
    <p>{length(@items)} items in cart</p>
    """
  end
end
```

Mount it with the `local_component/1` component — typically from a server
`LiveView` — passing assigns inline, just like `Phoenix.Component.live_component/1`:

```elixir
<.local_component module="Cart" items={@items} />
```

On mount, `mount/1` runs once and then `update/2` receives the assigns. They
arrive with atom keys at the top level (nested maps keep string keys, since they
are serialized to JSON to cross into the runtime).

`update/2` also runs again whenever the hosting server `LiveView` re-renders with
changed assigns, so the local component tracks server-side state:

```elixir
# in the server LiveView
def handle_event("add", _params, socket) do
  {:noreply, update(socket, :items, &[new_item() | &1])}
end

def render(assigns) do
  ~H"""
  <.local_component module="Cart" items={@items} />
  """
end
```

Each time `@items` changes, `Cart`'s `update/2` runs in the browser with the new
list.

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
