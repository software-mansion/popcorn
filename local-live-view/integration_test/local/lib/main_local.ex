defmodule MainLocal do
  # The main view of a live_local page: gets handle_params/3 and owns navigation.
  use LocalLiveView

  alias Phoenix.LiveView.JS

  @impl true
  def mount(_params, _session, socket) do
    {:ok,
     assign(socket,
       tab: nil,
       uri: nil,
       path: nil,
       params_calls: 0,
       clicks: 0,
       link_clicks: 0,
       form_value: "",
       submitted: ""
     )}
  end

  @impl true
  def handle_params(params, uri, socket) do
    {:noreply,
     assign(socket,
       tab: params["tab"] || "none",
       uri: uri,
       path: path(uri),
       params_calls: socket.assigns.params_calls + 1
     )}
  end

  @impl true
  def handle_event("patch", _params, socket) do
    {:noreply, push_patch(socket, to: "#{socket.assigns.path}?tab=pushed")}
  end

  def handle_event("patch_replace", _params, socket) do
    {:noreply, push_patch(socket, to: "#{socket.assigns.path}?tab=replaced", replace: true)}
  end

  def handle_event("navigate", _params, socket) do
    {:noreply, push_navigate(socket, to: "/hosted")}
  end

  def handle_event("inc", _params, socket) do
    {:noreply, update(socket, :clicks, &(&1 + 1))}
  end

  def handle_event("link_clicked", _params, socket) do
    {:noreply, update(socket, :link_clicks, &(&1 + 1))}
  end

  def handle_event("change", %{"value" => value}, socket) do
    {:noreply, assign(socket, :form_value, value)}
  end

  def handle_event("submit", %{"value" => value}, socket) do
    {:noreply, assign(socket, :submitted, value)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div id="main-local">
      <p id="main-tab">tab: {@tab}</p>
      <p id="main-uri">{@uri}</p>
      <p id="main-params-calls">params calls: {@params_calls}</p>
      <.link id="main-link-a" patch={"#{@path}?tab=a"}>tab a</.link>
      <.link id="main-link-replace" patch={"#{@path}?tab=link-replaced"} replace>replace</.link>
      <.link id="main-link-js" patch={"#{@path}?tab=js"} phx-click={JS.push("link_clicked")}>
        link clicks: {@link_clicks}
      </.link>
      <button id="main-patch" phx-click="patch">push_patch</button>
      <button id="main-patch-replace" phx-click="patch_replace">push_patch replace</button>
      <button id="main-navigate" phx-click="navigate">push_navigate</button>
      <button id="main-inc" phx-click="inc">clicks: {@clicks}</button>
      <form id="main-form" phx-change="change" phx-submit="submit">
        <input id="main-input" name="value" value={@form_value} />
        <button id="main-submit" type="submit">submit</button>
      </form>
      <p id="main-form-value">value: {@form_value}</p>
      <p id="main-submitted">submitted: {@submitted}</p>
    </div>
    """
  end

  # URI.parse/1 needs regexes, which AtomVM doesn't support.
  defp path(uri) do
    [_scheme, rest] = String.split(uri, "://", parts: 2)

    case String.split(rest, "/", parts: 2) do
      [_authority, path] -> "/" <> hd(String.split(path, ["?", "#"], parts: 2))
      [_authority] -> "/"
    end
  end
end
