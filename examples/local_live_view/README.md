# LocalLiveView

**This is a POC of LocalLiveView the LiveView that implements LiveView functionality 
in the browser using Popcorn**

Usage:

1. Define your LiveView inside the lib directory. Remember to use pop-click instead of phx-click attribute if you want to catch click event.

example:
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
    socket =
      socket
      |> assign(:temperature, temperature)
    {:ok, socket}
  end

  def handle_event("inc_temperature", _params, socket) do
    temperature = socket.assigns.temperature
    socket = Phoenix.LiveView.Utils.assign(socket, :temperature, temperature+1)
    {:noreply, socket}
  end
end
```
2. Place your view inside the .html file, using data-pop-view attribute.

example (index.html):
```html
<html>
    <style>
        body {
            font-size: 1.5em;
            font-family: sans-serif;
            max-width: 800px;
            padding: 1em;
            background-color: #161616;
            color: #fcfcfc;
        }
    </style>
    <script type="module">
        import { Popcorn } from "./wasm/popcorn.js";
    </script>
    <script type="module" src="local_live_view.js" defer></script>
    <body>
    <div data-pop-view="ThermostatLive"></div>
    </body>
</html>
```

3. Run:
```bash
mix deps.get
mix popcorn.cook
elixir server.exs
```

and visit [localhost:4000](http://localhost:4000)
