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

  test "leaves the mount point empty when rendered by a connected host LiveView" do
    # What Phoenix.LiveView.Channel puts in the process dictionary of a LiveView
    Process.put(:"$process_label", {Phoenix.LiveView, __MODULE__, "lv:phx-1"})
    assert Phoenix.LiveView.Debug.liveview_process?(self())

    html = render(%{view: inspect(Hello), name: "world"})
    assert mount_point(html) == ~s(<div id="llv-#{id(Hello)}" data-pop-root></div>)
  end

  test "leaves the mount point empty with ssr={false}" do
    html = render(%{view: inspect(Hello), name: "world", ssr: false})
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

  test "does not forward ssr and url to the view" do
    assert %{id: "x", name: "world"} ==
             Component.comp_assigns(%{
               id: "x",
               view: "V",
               name: "world",
               ssr: true,
               url: "http://localhost/",
               __changed__: nil
             })
  end

  describe "mirrored view" do
    test "renders on the server in the dead render" do
      socket = %Socket{id: "phx-1"}
      assigns = %{id: "m", view: inspect(Mirrored), name: "there"}

      assert {:ok, socket} = Component.Mirrored.update(assigns, socket)
      assert IO.iodata_to_binary(socket.assigns.ssr_html) == "<p>Mirrored there</p>"
    end

    test "does not render on the server when connected" do
      # An already signed mirror token, so update/2 doesn't need an endpoint
      socket = %Socket{
        id: "phx-1",
        transport_pid: self(),
        assigns: %{__changed__: %{}, mirror_token: "signed"}
      }

      assigns = %{id: "m", view: inspect(Mirrored), name: "there"}

      assert {:ok, socket} = Component.Mirrored.update(assigns, socket)
      assert socket.assigns.ssr_html == nil
    end
  end

  defp id(module), do: String.replace(inspect(module), ".", "-")
end
