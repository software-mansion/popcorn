# Mirror Sync

By default, a LocalLiveView is fully self-contained in the browser — the server knows nothing about its state. Mirror Sync is the mechanism for bridging that gap: it lets a LocalLiveView push selected assigns to the server, where a server-side module can react to them.

## When to use it

Use Mirror Sync when you need the server to be aware of local state, for example:

- Broadcasting local state changes to other connected users via PubSub
- Persisting user input to a database
- Letting a server-side Phoenix LiveView display or react to local state

## How it works

```
Browser                          Server
┌─────────────────────┐          ┌───────────────────────────┐
│  MyLocal            │          │  Mirror.MyLocal           │
│  handle_event(...)  │          │  handle_sync(             │
│    mirror_sync(     │──sync───▶│    local_assigns,         │
│      socket,        │          │    mirror_assigns         │
│      [:count]       │          │  )                        │
│    )                │          └───────────────────────────┘
└─────────────────────┘
```

1. Your LocalLiveView calls `mirror_sync/2` with the socket and a list of assign keys.
2. The JS bridge sends those assigns to the server over a Phoenix Channel.
3. The server finds `Mirror.<ViewName>` and calls its `handle_sync/2` callback.

## Setting up mirror sync

### 1. Declare the mirror keys

Pass the keys you want to sync when calling `mirror_sync/2`:

```elixir
defmodule MyLocal do
  use LocalLiveView

  def mount(_params, _session, socket) do
    {:ok, assign(socket, count: 0)}
  end

  def render(assigns) do
    ~H"""
    <button phx-click="increment">Count: {@count}</button>
    """
  end

  def handle_event("increment", _params, socket) do
    socket = update(socket, :count, &(&1 + 1))
    mirror_sync(socket, [:count])
    {:noreply, socket}
  end
end
```

`mirror_sync/2` takes the socket and a list of assign keys to send. It returns the socket unchanged, so you can pipe it or ignore the return value.

### 2. Create the Mirror module

On the server side, create `lib/mirror/my_live.ex`:

```elixir
defmodule Mirror.MyLocal do
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(local_assigns, _mirror_assigns) do
    {:ok, local_assigns}
  end
end
```

The module must be named `Mirror.<ViewName>` — the view name is the last part of your LocalLiveView module name. `LocalLiveView.Component` auto-detects the mirror module and enables the sync channel when rendering the mount point.

`handle_sync/2` receives:
- `local_assigns` — a map of the synced assigns (keys are strings)
- `mirror_assigns` — the mirror's current state (what was returned from the previous `handle_sync` call)

It must return `{:ok, new_mirror_assigns}`.

## Broadcasting via PubSub

The most common use case is broadcasting local state to other LiveViews:

```elixir
defmodule Mirror.MyLocal do
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(local_assigns, _mirror_assigns) do
    Phoenix.PubSub.broadcast(
      MyApp.PubSub,
      "llv_mirror:MyLocal",
      {:llv_attrs, local_assigns}
    )

    {:ok, local_assigns}
  end
end
```

Then subscribe and handle in a server-side LiveView:

```elixir
defmodule MyAppWeb.DashboardLive do
  use MyAppWeb, :live_view

  def mount(_params, _session, socket) do
    Phoenix.PubSub.subscribe(MyApp.PubSub, "llv_mirror:MyLocal")
    {:ok, assign(socket, count: 0)}
  end

  def handle_info({:llv_attrs, %{"count" => count}}, socket) do
    {:noreply, assign(socket, count: count)}
  end
end
```

## Mirror assigns and conflict resolution

`handle_sync/2` can return different assigns than it received — this is useful when you want the mirror to store derived or enriched state:

```elixir
def handle_sync(%{"count" => count} = local_assigns, mirror_assigns) do
  enriched = Map.put(local_assigns, "total_increments", Map.get(mirror_assigns, "total_increments", 0) + 1)
  {:ok, enriched}
end
```

The returned value becomes `mirror_assigns` in the next call, but it is **not** sent back to the browser — it only lives on the server.

## Sync frequency

`mirror_sync/2` sends over the wire every time it's called. Call it only when state has changed and the server needs to know — typically at the end of `handle_event/3` when relevant assigns were updated.

## Serialization

Assigns are serialized as a JSON-compatible map before being sent. The following types are supported:

- Primitives: strings, numbers, booleans, nil
- Lists
- Maps (keys are converted to strings)
- Structs (converted to maps via `Map.from_struct/1`)

Atoms, tuples, and PIDs are not serializable and will cause a runtime error if included.
