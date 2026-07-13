# Your first LocalLiveView

This guide walks through building a simple counter view to introduce the LocalLiveView programming model.

## Creating a view module

LocalLiveView modules live in the `local/lib/` directory of your project. Create `local/lib/counter_local.ex`:

```elixir
defmodule CounterLocal do
  use LocalLiveView

  # Runs once, in the browser, when the view is mounted — the counter starts at zero.
  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  # Renders the current count. Runs again after each event, without touching the server.
  def render(assigns) do
    ~H"""
    <div>
      <p>Count: {@count}</p>
      <button phx-click="increment">+</button>
      <button phx-click="decrement">-</button>
    </div>
    """
  end

  # Handles the clicks from the two buttons above. Each one updates :count,
  # which triggers a re-render.
  def handle_event("increment", _params, socket) do
    {:noreply, update(socket, :count, &(&1 + 1))}
  end

  def handle_event("decrement", _params, socket) do
    {:noreply, update(socket, :count, &(&1 - 1))}
  end
end
```

This should look familiar if you've used Phoenix LiveView. The only difference is `use LocalLiveView` instead of `use Phoenix.LiveView`.

`LocalLiveView` documents each callback in full, including the ones this counter does not need — `update/2` for assigns coming from the host LiveView, and `handle_info/2` for messages the view sends itself.

## Mounting the view

Use the `<.local_live_view>` component in any Phoenix template:

```heex
<.local_live_view view="CounterLocal" />
```

The `view` attribute is the module name as a string. The component renders a `<div>` that becomes the mount point for the Wasm view.

The counter is now fully local — clicks are handled in the browser with no server round-trips.

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

Each `<.local_live_view>` on the page runs as an independent process in the Wasm runtime. You can mount as many as you need:

```heex
<.local_live_view view="CounterLocal" />
<.local_live_view view="CounterLocal" id="second-counter" />
<.local_live_view view="ThermostatLocal" />
```

When mounting the same view multiple times, use the `id` attribute to give each instance a unique identifier.
