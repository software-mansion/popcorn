defmodule LocalLiveView.ComponentTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog

  alias LocalLiveView.Component
  alias Phoenix.LiveView.Socket

  defmodule Hello do
    use LocalLiveView

    @impl true
    def mount(_params, _session, socket), do: {:ok, assign(socket, :name, "nobody")}

    @impl true
    def render(assigns) do
      ~H"""
      <p>Hello, {@name}</p>
      """
    end
  end

  defmodule Broken do
    use LocalLiveView

    @impl true
    def mount(_params, _session, _socket), do: raise("browser only")

    @impl true
    def render(assigns), do: ~H"never"
  end

  # Mirror.<view> modules make the component render as a live component
  defmodule Mirror.Mirrored do
    use LocalLiveView.Mirror

    @impl true
    def handle_sync(local_assigns, _mirror_assigns, _session), do: {:ok, local_assigns}
  end

  defmodule Mirrored do
    use LocalLiveView

    @impl true
    def render(assigns) do
      ~H"""
      <p>Mirrored {@name}</p>
      """
    end
  end

  # HEEx passes __changed__: nil to a function component on a full render
  defp render(assigns) do
    assigns
    |> Map.put_new(:__changed__, nil)
    |> Component.local_live_view()
    |> Phoenix.HTML.Safe.to_iodata()
    |> IO.iodata_to_binary()
  end

  defp mount_point(html) do
    [_, mount_point] = Regex.run(~r/(<div id="[^"]*" data-pop-root[^>]*>.*?<\/div>)/s, html)
    mount_point
  end

  test "fills the mount point with the server-rendered view on a full render" do
    html = render(%{view: inspect(Hello), name: "world"})

    assert mount_point(html) ==
             ~s(<div id="llv-#{id(Hello)}" data-pop-root data-pop-ssr inert><p>Hello, world</p></div>)
  end

  test "leaves the mount point empty on a re-render of the host" do
    html = render(%{view: inspect(Hello), name: "world", __changed__: %{name: true}})
    assert mount_point(html) == ~s(<div id="llv-#{id(Hello)}" data-pop-root></div>)
  end

  test "fills the mount point again when a re-render mounts another view" do
    html = render(%{view: inspect(Hello), name: "world", __changed__: %{view: true}})
    assert mount_point(html) =~ "<p>Hello, world</p>"
  end

  test "leaves the mount point empty with llv_ssr={false}" do
    html = render(%{view: inspect(Hello), name: "world", llv_ssr: false})
    assert mount_point(html) == ~s(<div id="llv-#{id(Hello)}" data-pop-root></div>)
    refute html =~ "ssr"
  end

  test "leaves the mount point empty when the view module is not available" do
    html = render(%{view: "Nope.Missing", name: "world"})
    assert mount_point(html) == ~s(<div id="llv-Nope-Missing" data-pop-root></div>)
  end

  test "logs and leaves the mount point empty when the view raises on the server" do
    log =
      capture_log(fn ->
        html = render(%{view: inspect(Broken)})
        assert mount_point(html) == ~s(<div id="llv-#{id(Broken)}" data-pop-root></div>)
      end)

    assert log =~ "rendering on the server failed"
    assert log =~ "browser only"
  end

  test "does not forward the component's own attributes and state to the view" do
    assert %{id: "x", name: "world"} ==
             Component.comp_assigns(%{
               id: "x",
               view: "V",
               name: "world",
               llv_ssr: true,
               llv_url: "http://localhost/",
               __llv__: %{new_mount_point: true},
               __changed__: nil
             })
  end

  describe "mirrored view" do
    # An already signed mirror token, so update/2 doesn't need an endpoint
    defp connected_socket do
      %Socket{
        id: "phx-1",
        transport_pid: self(),
        assigns: %{__changed__: %{}, mirror_token: "signed"}
      }
    end

    # local_live_view/1 decides whether the mount point is new, the same way
    # as for a plain view, and passes it under __llv__; the component acts on it.
    defp assigns(new_mount_point?) do
      %{
        id: "m",
        view: inspect(Mirrored),
        name: "there",
        __llv__: %{new_mount_point: new_mount_point?}
      }
    end

    test "renders on the server in the dead render" do
      socket = %Socket{id: "phx-1"}

      assert {:ok, socket} = Component.Mirrored.update(assigns(true), socket)
      assert IO.iodata_to_binary(socket.assigns.ssr_html) == "<p>Mirrored there</p>"
    end

    test "renders on the server when connected, for a new mount point (live navigation)" do
      assert {:ok, socket} = Component.Mirrored.update(assigns(true), connected_socket())
      assert IO.iodata_to_binary(socket.assigns.ssr_html) == "<p>Mirrored there</p>"
    end

    test "does not render on the server on later updates" do
      assert {:ok, socket} = Component.Mirrored.update(assigns(true), connected_socket())
      assert {:ok, socket} = Component.Mirrored.update(assigns(false), socket)
      assert socket.assigns.ssr_html == nil
    end
  end

  defp id(module), do: String.replace(inspect(module), ".", "-")
end
