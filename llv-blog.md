# The first release of Local Live View!

Today marks another milestone in the Local Live View development. In April, Franek Kubis announced it at ElixirConf EU, and we published several demos and POCs since then. Now, it finally has docs, reasonable API (more or less :P) and a Hex release. It's not been run in prod yet, but we're close already. We deployed some demos, too. So, it's the perfect time to start hacking around.

## Why Local Live View?

Local Live View makes it possible to manage local state the same way as you do with the server state in a regular Live View. While Live View provides JS hooks and JS commands, they're arguably more complex and harder to maintain than regular Elixir code. And you're often forced to write JS.

Thus, Local Live View allows you to run Live Views in the browser via Popcorn, keeping all their assigns locally. You can now freely choose what should stay on the client, and what needs to reach the server. It helps, for example, offload the server from handling simple UI updates, drastically reduce latency on poor networks, and avoid 'WebSocket disconnected' issues.

## How do I use it?

Let's say you have a simple live component:

```elixir
defmodule MyAppWeb.ThermostatComponent do
  use Phoenix.LiveComponent

  @impl true
  def update(assigns, socket) do
    {:ok, assign(socket, :temperature, assigns.temperature)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div>
      Current temperature: {@temperature}°C
      <button phx-click="inc_temperature" phx-target={@myself}>+</button>
    </div>
    """
  end

  @impl true
  def handle_event("inc_temperature", _params, socket) do
    {:noreply, update(socket, :temperature, &(&1 + 1))}
  end
end
```

rendered from a Live View:

```elixir
defmodule MyAppWeb.MyLive do
  use Phoenix.LiveView

  @impl true
  def render(assigns) do
    ~H"""
    <.live_component module={MyAppWeb.ThermostatComponent} id="thermostat" temperature={25} />
    """
  end
end
```

Now, moving the component to the client requires only slight changes. First, we `use LocalLiveView`, and change the module name to reflect that:

```diff
- defmodule MyAppWeb.ThermostatComponent do
-   use Phoenix.LiveComponent
+ defmodule MyAppWeb.ThermostatLocal do
+   use LocalLiveView
```

Then, we need to remove `phx-target={@myself}` for the `click` event - Local Live View events always target themselves, not their parents:

```diff
- <button phx-click="inc_temperature" phx-target={@myself}>+</button>
+ <button phx-click="inc_temperature">+</button>
```

Thermostat can now be rendered with:

```diff
- <.live_component module={MyAppWeb.ThermostatComponent} id="thermostat" temperature={25} />
+ <.local_live_view view="MyAppWeb.ThermostatLocal" id="thermostat" temperature={25} />
```

The only step left is to move the Thermostat to the right place in your project, and voila! The Thermostat is now fully local.

Notice that we changed the Thermostat from a Live Component to a Local Live View. While the API is similar, they're different under the hood: Local Live View runs in a separate process (and even on a separate VM). All the differences are outlined in the docs, the most important being the communication, which now goes across the network. For that, you can use one of two mechanisms: push_server_event or mirror sync - they're thoroughly explained in the docs as well.

## See it in action

This post shows a very simple example, but Local Live View is already capable of running more complex apps. We prepared several demos to explain it:

- Kanban boards - an app that allows you to create and browse kanban-style boards. The most interesting part is the board view: it's 100% Elixir, supports drag&drop, optimistic updates, and all the forms are handled locally. Open the app, create a board, then disconnect from the network and see how it behaves. It also demonstrates server synchronization via `push_server_event` - reconnect and open two windows side-by-side to see it.

- Pong game - a simple, 100% local, 100% Elixir game - you play pong with a bot.

- Burrito order form - a fairly complex form demo, comparing regular Live View and Local Live View side-by-side. It also demonstrates synchronizing state via mirror sync.

## The future

Even though Local Live View has been pretty stable for us recently, we're going to test it further in different scenarios and see how it behaves. We're going to polish the API along the way, but with no big changes expected.

Removing any dependencies on Live View private APIs is another important point. Fortunately, we're well on our way to doing that and we're working closely with the Live View team.

Reducing the bundle size is something we constantly keep working on. We recently introduced an experimental Elixir tree-shaking tool that already helps significantly, with the Kanban demo's size down to < 1.5 MB compressed.

## Go use it!

With the first release, Local Live View is more than ready for you to try! Here's the docs, the repo, and examples. Happy hacking!