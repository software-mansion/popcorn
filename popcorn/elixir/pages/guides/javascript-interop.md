# Use browser APIs from Elixir

`Popcorn.Wasm.run_js/3` runs a JavaScript function in the browser page. The
calling BEAM process waits for the result.

## Return a value

```elixir
{:ok, language} =
  Popcorn.Wasm.run_js(
    """
    () => navigator.language
    """,
    %{}
  )
```

The function receives the argument map as its first argument. Popcorn converts
the returned value to a BEAM term.

Use `run_js!/3` when a JavaScript error must raise an Elixir exception.

## Use arguments

Pass data separately from the function source:

```elixir
Popcorn.Wasm.run_js!(
  """
  ({id, text}) => {
    const statusNode = document.querySelector(id);
    statusNode.textContent = text;
  }
  """,
  %{id: "#status", text: "Ready"}
)
```

Do not build JavaScript source with string interpolation. Separate arguments
avoid quoting errors and code injection.

## Call back into BEAM

The second function argument contains bridge actions:

```elixir
Popcorn.Wasm.run_js!(
  """
  ({target}, {send}) => {
    const refreshNode = document.querySelector("#refresh");

    refreshNode.addEventListener("click", () => {
      void send(target, {event: "refresh"});
    });
  }
  """,
  %{target: self()}
)
```

The BEAM process receives `{:wasm, %{"event" => "refresh"}}`.

The action object also provides `call` and `cast`. Those actions require a
running `Popcorn.Proxy`.

## Keep a JavaScript object

Return a tracked value for a DOM node or another object:

```elixir
element =
  Popcorn.Wasm.run_js!(
    """
    () => {
      const chartNode = document.querySelector("#chart");

      return new TrackedValue(chartNode);
    }
    """,
    %{}
  )

Popcorn.Wasm.run_js!(
  "({element}) => element.replaceChildren()",
  %{element: element}
)
```

Add an idempotent cleanup function when the object owns a listener, timer, or
other resource.

## Avoid deadlocks

`run_js/3` blocks only the calling BEAM process. Other processes continue to
run.

Do not let the JavaScript function call the same GenServer that waits for
`run_js/3`. Neither side can complete in that cycle.

The current bridge evaluates JavaScript source. The page Content Security
Policy (CSP) must permit `unsafe-eval`.
