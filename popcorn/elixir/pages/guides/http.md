# Making HTTP requests

# Send HTTP requests

Use `Popcorn.Fetch` when Elixir code needs HTTP in the browser. It uses the
browser `fetch()` API. You can Use it directly or through Req.

## Using `Req`

Add Req to the application dependencies, we support Req from 0.5 to 0.8.

The adapter is compiled only when Req is available. If you add Req after
Popcorn has already compiled, recompile it with `mix deps.compile popcorn --force`.

Popcorn installs its Req adapter when ran in the browser. If you already customized the adapter, Popcorn won't install its own.

```elixir
response = Req.get!("https://api.example.com/status")
```

You can also select the adapter for one request:

```elixir
response =
  Req.get!(
    "https://api.example.com/status",
    adapter: Popcorn.Fetch.adapter()
  )
```

The adapter supports normal response bodies, `:into` functions, collectables,
and `:self` streams. It buffers request bodies before upload.

## Use Popcorn.Fetch directly

Use `request/2` when the application does not depend on Req:

```elixir
{:ok, response} =
  Popcorn.Fetch.request(%{
    method: "GET",
    url: "/api/status"
  })

IO.inspect(response.status)
IO.inspect(response.body)
```

An HTTP error status still returns `{:ok, response}`. Network failures and
timeouts return `{:error, reason}`.

## Browser limits

Cross-origin requests need permission from the target server's CORS policy.
The browser controls cookies, redirects, restricted headers, and response
decompression.

Popcorn cannot preserve compressed response bytes. It also cannot set headers
such as `Host` or `Cookie` directly.
