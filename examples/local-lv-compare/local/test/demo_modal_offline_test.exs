defmodule DemoModalOfflineTest do
  @moduledoc """
  Runs the offline (LocalLiveView) modal: mounts the view, handles the close
  event and renders both states to HTML. Popcorn runs the same BEAM in the
  browser, so the view behaves there as it does here.
  """
  use ExUnit.Case

  @modal ~s(class="modal")

  test "DemoModalOffline mount/handle_event/render" do
    {:ok, socket} = DemoModalOffline.mount(%{}, %{}, %Phoenix.LiveView.Socket{})
    {:noreply, closed} = DemoModalOffline.handle_event("close_modal", %{}, socket)

    assert render(socket) =~ @modal
    refute render(closed) =~ @modal
  end

  defp render(socket) do
    socket.assigns
    |> Map.put(:__changed__, nil)
    |> DemoModalOffline.render()
    |> Phoenix.HTML.Safe.to_iodata()
    |> IO.iodata_to_binary()
  end
end
