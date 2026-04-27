# Your first LocalLiveView

This guide walks through building a simple counter view to introduce the LocalLiveView programming model.

## Creating a view module

LocalLiveView modules live in the `local/lib/` directory of your project. Create `local/lib/counter_local.ex`:

```elixir
defmodule CounterLocal do
  use LocalLiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def render(assigns) do
    ~H"""
    <div>
      <p>Count: {@count}</p>
      <button phx-click="increment">+</button>
      <button phx-click="decrement">-</button>
    </div>
    """
  end

  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end
end
```

This should look familiar if you've used Phoenix LiveView. The differences are:

- `use LocalLiveView` instead of `use Phoenix.LiveView`

## Mounting the view

Use the `<.local_live_view>` component in any Phoenix template:

```heex
<.local_live_view view="CounterLocal" />
```

The `view` attribute is the module name as a string. The component renders a `<div>` that becomes the mount point for the WASM view.

## Building and running

After adding the module, rebuild the WASM bundle:

```bash
mix llv.build
```

Then start or reload the server:

```bash
mix phx.server
```

The counter is now fully local — clicks are handled in the browser with no server round-trips.

## Callbacks

### `mount/3`

Called once when the view is initialized. Use it to set up initial assigns.

```elixir
def mount(_params, _session, socket) do
  {:ok, assign(socket, count: 0, label: "Counter")}
end
```

### `render/1`

Returns the HEEx template for the current state. Called automatically after every state change.

```elixir
def render(assigns) do
  ~H"""
  <p>{@label}: {@count}</p>
  """
end
```

### `handle_event/3`

Handles events triggered from the template. Returns `{:noreply, socket}` with updated assigns.

```elixir
def handle_event("reset", _params, socket) do
  {:noreply, assign(socket, count: 0)}
end
```

## Assigning state

LocalLiveView uses the same assign functions as Phoenix LiveView:

```elixir
# Assign a single key
assign(socket, :count, 0)

# Assign multiple keys at once
assign(socket, count: 0, label: "Counter")

# Update a key using the current value
update(socket, :count, &(&1 + 1))
```

## Timers and periodic updates

You can schedule recurring messages using `Process.send_after/3`, just like in Phoenix LiveView:

```elixir
def mount(_params, _session, socket) do
  Process.send_after(self(), :tick, 1000)
  {:ok, assign(socket, time: Time.utc_now())}
end

def handle_info(:tick, socket) do
  Process.send_after(self(), :tick, 1000)
  {:noreply, assign(socket, time: Time.utc_now())}
end
```

## Multiple views on one page

Each `<.local_live_view>` on the page runs as an independent process in the WASM runtime. You can mount as many as you need:

```heex
<.local_live_view view="CounterLocal" />
<.local_live_view view="CounterLocal" id="second-counter" />
<.local_live_view view="ThermostatLocal" />
```

When mounting the same view multiple times, use the `id` attribute to give each instance a unique identifier.
