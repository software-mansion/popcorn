> This guide assumes you have Popcorn installed. See [Installation](installation.md) for instructions.

Let's create a small "Hello world!" example with Popcorn.

## Add a HTML template

Create a `priv/static` directory, where our static assets will live:

```
mkdir -p priv/static
```

And then an `index.html` file, with JavaScript logic to run Popcorn:

```html
<html>
  <body>
    <p id="status">Starting Popcorn...</p>

    <script type="module">
      import { Popcorn } from "./popcorn/index.mjs";

      const { ok, data, error } = await Popcorn.init();
      if (!ok) {
        const status = document.querySelector("#status");
        status.textContent = "Popcorn failed to start.";

        throw error;
      }
    </script>
  </body>
</html>
```

## Add a function to change the HTML

Edit the `lib/foo.ex` and add a function to run JS from Elixir side:

```elixir
alias Popcorn.Wasm

def set_html do
  with :ok <- Wasm.await_ready() do
    Wasm.run_js!("""
      () => {
        const status = document.querySelector("#status");

        status.textContent = "Hello world!";
      }
    """)

    :ok
  end
end
```

And let's add a `Task` to run it from the supervision tree in `application.ex`:

```diff
   @impl true
   def start(_type, _args) do
     children = [
-      # Starts a worker by calling: Foo.Worker.start_link(arg)
-      # {Foo.Worker, arg}
+      {Task, &Foo.set_html/0}
     ]

     # See https://hexdocs.pm/elixir/Supervisor.html
```

## Start the runtime

Start the local server:

```
mix popcorn.dev
```

and access [localhost:8000](localhost:8000).

To learn more, check "Guides" section. Good place to start is [JS and Elixir communication](TODO).
