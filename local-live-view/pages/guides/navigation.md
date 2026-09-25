# Navigation

Local LiveView supports navigation. Navigating to another page, with
`<.link navigate={...}>` or `<.link href={...}>`, goes through the server as
usual. `<.link patch={...}>` triggers `handle_params/3` for the main view
of the page, which can be either a local or regular view.

The main local view is the one mounted at the router, with
`LocalLiveView.Router.live_local/2`:

```elixir
live_local "/dashboard", DashboardLocal
```

If a non-main view needs the URL params, have the main one pass them down as
assigns.

## `handle_params/3`

The main view opts into navigation by exporting `handle_params/3`, mirroring
`Phoenix.LiveView`. It receives the current query params and URL.

```elixir
def handle_params(params, uri, socket) do
  {:noreply, assign(socket, :tab, params["tab"] || "home")}
end
```

It runs at mount with the initial query params, and again on every patch
of the page URL. It also runs when the view is rendered on the server.

## `push_patch/2`

`LocalLiveView.push_patch/2` mirrors `Phoenix.LiveView.push_patch/2` and is used
to navigate from Elixir:

```elixir
def handle_event("select_tab", %{"tab" => tab}, socket) do
  {:noreply, push_patch(socket, to: "/dashboard?tab=#{tab}")}
end
```

If the page's main view is local, the patch is handled without the server round
trip.

## `redirect/2` and `push_navigate/2`

`LocalLiveView.redirect/2` and `LocalLiveView.push_navigate/2` mirror their `Phoenix.LiveView` counterparts.

```elixir
def handle_event("checkout", _params, socket) do
  {:noreply, redirect(socket, to: "/checkout")}
end
```

The difference from server-side redirect/push_navigate is that Flash isn't
carried over to the next page. If that's required, use `LocalLiveView.push_server_event/3`
and navigate from the server.
