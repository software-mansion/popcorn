> This guide assumes you have Popcorn installed. See [Installation](installation.md) for instructions.

While [printing to the HTML](hello-world.md) is great, you will need more things when building even a simple application.

Let's explore more Popcorn APIs and how they fit together!

## Setup

Don't you love creating a simple counter app to check how a library does things? How about counter of counters app?

Start with creating a new project:

```bash
mix new counter --sup
```

We'll work with the following HTML file, which includes a static markup for main counter and a template for creating a sub-counters:

```
mkdir -p priv/static
touch priv/static/index.html
```

```html
<html>
  <head>
    <style>
      body {
        max-width: 640px;
        margin: 2rem auto;
        padding: 0 1rem;
      }

      header {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        font-size: 1.5rem;
      }

      #counters {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(200px, 1fr));
        gap: 1.5rem;
        margin-top: 2rem;
      }

      fieldset {
        border: 2px solid #444;
        padding: 1rem;
      }

      button {
        width: 2.5rem;
        height: 2.5rem;
        font-size: 1.25rem;
      }
    </style>
  </head>
  <body>
    <header>
      <span>Counters count: <output id="total">0</output></span>
      <button id="add">+</button>
      <button id="remove">-</button>
    </header>

    <main id="counters"></main>

    <template id="counter">
      <fieldset>
        <legend>counter <span class="id"></span></legend>
        <p>count: <output class="count">0</output></p>
        <button class="inc">+</button>
        <button class="dec">-</button>
      </fieldset>
    </template>

    <script type="module">
      import { Popcorn } from "./popcorn/index.mjs";

      const { ok, data, error } = await Popcorn.init();
      if (!ok) throw error;
    </script>
  </body>
</html>
```

## Handling main counter

First, let's create some utils to manipulate HTML:

```elixir
defmodule Counters.HTML do
  def by_selector(selector, opts \\ []) do
    """
    ({ selector, root }) => {
      const node = root ?? document;

      return node.querySelector(selector);
    }
    """
    |> Wasm.run_js!(%{selector: selector, root: Keyword.get(opts, :root)})
  end

  def by_id(id, opts \\ []), do: by_selector("##{id}" ,opts)

  def on_click(node, pid, event) do
    """
    ({ node, pid, event }, { send }) => {
      node.onclick = () => send(pid, event);
    }
    """
    |> Wasm.run_js!(%{node: node, pid: pid, event: event})
  end

  def set(node, value) do
  """
        ({ node, value }) => {
          node.textContent = value;
          return node;
        }
        """
        |>Wasm.run_js!( %{node: node, value: value})
  end

  def clone_counter_html(counter_id) do
  counter = "counter-tmpl"
  |> by_id()
  |> clone_node()
  |> tap(fn counter -> ".id" |> by_selector(root: counter) |> set(counter_id) end)

"counters"
by_id()
|> append_child(counter)
  end

  def append_child(node, child_node) do

  end
end
```
