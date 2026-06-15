defmodule DemoModalOfflineAtomVMTest do
  @moduledoc """
  Runs the offline (LocalLiveView) modal on a real native AtomVM, the same runtime
  that powers the example in the browser. We compile a quoted expression that mounts
  the view, handles the close event and renders both states to HTML, run it on the
  AtomVM binary, and assert on the HTML it produces.

  Build the runtime once with:

      MIX_TARGET=wasm mix popcorn.build_runtime --target unix
  """
  use ExUnit.Case
  alias Popcorn.Support.AtomVM

  @moduletag :tmp_dir
  @moduletag timeout: :timer.minutes(3)

  @modal ~s(class="modal")

  test "DemoModalOffline mount/handle_event/render runs on AtomVM", %{tmp_dir: dir} do
    bundle =
      quote do
        render = fn socket ->
          socket.assigns
          |> Map.put(:__changed__, nil)
          |> DemoModalOffline.render()
          |> Phoenix.HTML.Safe.to_iodata()
          |> IO.iodata_to_binary()
        end

        {:ok, socket} = DemoModalOffline.mount(%{}, %{}, %Phoenix.LiveView.Socket{})
        open_html = render.(socket)

        {:noreply, closed} = DemoModalOffline.handle_event("close_modal", %{}, socket)
        closed_html = render.(closed)

        %{open: open_html, closed: closed_html}
      end
      |> AtomVM.compile_quoted()

    assert %{open: open_html, closed: closed_html} = AtomVM.run(bundle, dir)

    # Same behaviour we asserted on the BEAM, now produced by AtomVM:
    assert open_html =~ @modal
    refute closed_html =~ @modal
  end
end
