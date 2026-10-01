# Build your first Popcorn application

You will build a supervised counter. JavaScript will call its GenServer and
show the returned value.

Complete [Installation](installation.html) before this tutorial.

## Create the counter

Create `lib/my_app/counter.ex`:

```elixir
defmodule MyApp.Counter do
  use GenServer

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, 0, name: :counter)
  end

  @impl true
  def init(count), do: {:ok, count}

  @impl true
  def handle_call(["add", amount], _from, count) do
    count = count + amount
    {:reply, count, count}
  end
end
```

The process owns the count. Its registered name lets JavaScript address it.
JavaScript arrays become BEAM lists, so the callback matches a list.

## Add the bridge to the supervision tree

Update `lib/my_app/application.ex`:

```elixir
defmodule MyApp.Application do
  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Popcorn.Proxy,
      MyApp.Counter
    ]

    Supervisor.start_link(children,
      strategy: :one_for_one,
      name: MyApp.Supervisor
    )
  end
end
```

`Popcorn.Proxy` connects JavaScript calls and casts to GenServers. A plain
`send()` does not use the proxy.

Compile the application again:

```console
mix compile
```

## Call the counter

Add this code after `Popcorn.init()` in the JavaScript entry point:

```typescript
const reply = await popcorn.genserver.call("counter", ["add", 2]);
if (!reply.ok) throw reply.error;

document.querySelector("#count").textContent = String(reply.data);
```

Add the target element to the page:

```html
<p>Count: <span id="count">0</span></p>
```

Start the Vite development server. The page shows `Count: 2` after Popcorn
starts.

## What happened

Vite packaged the application and its dependencies as browser assets. Popcorn
started the OTP application inside a Web Worker.

JavaScript sent a normal GenServer call through `Popcorn.Proxy`. The counter
updated its state and returned the new value.

Next, read [Send messages and use GenServers](messaging.html).
