# First steps

## Usage
To use LocalLiveView you need to:
1. Clone [Popcorn](https://github.com/software-mansion/popcorn) repo and define your own mix project.
2. Add `:local_live_view` to your mix.exs file:
```elixir
defmodule LocalThermostat.MixProject do
  use Mix.Project
  ...
  defp deps do
    [
      {:local_live_view, path: path_to_popcorn <> "/examples/local_live_view"}
    ]
  end
end
```
3. Attach local_live_view.js script in your .html file:
```html
<html>
    <script type="module" src="./local_live_view/local_live_view.js" defer></script>
    <body>
    </body>
</html>
```
4. Define your LocalLiveView in the `lib` directory:
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
5. Add html tag to render defined view, by using data-pop-view:
```html
<html>
    <script type="module" src="./local_live_view/local_live_view.js" defer></script>
    <body>
    <div data-pop-view="ThermostatLive"></div>
    </body>
</html>
```

## Build
To build the project after defining the above run:
```bash
mix deps.get
mix popcorn.cook
```
This will generate the necessary scripts and popcorn files into the `static` directory in your project.

## Serve

To properly serve a project check the [Popcorn documentation](https://hexdocs.pm/popcorn/0.1.0/readme.html#getting-started). You can use `mix popcorn.dev_server` and run with `elixir dev_server.exs`
