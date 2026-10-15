defmodule FormDemoLocalTest do
  @moduledoc """
  Runs the offline (LocalLiveView) form: mounts the view, runs the validate/save
  events and renders the resulting states to HTML. Popcorn runs the same BEAM in
  the browser, so the view behaves there as it does here.

  `save` calls `LocalLiveView.mirror_sync/2`, which in the browser pushes the
  synced assigns to the server-side mirror. The socket here isn't connected, so
  it does nothing, while the local state transitions run exactly as they do in
  the browser.
  """
  use ExUnit.Case

  test "FormDemoLocal mount/validate/save/render" do
    {:ok, socket} = FormDemoLocal.mount(%{}, %{}, %Phoenix.LiveView.Socket{})

    # Events arrive form-wrapped, exactly as Phoenix delivers phx-change/phx-submit.
    valid = %{"user" => %{"username" => "alice", "email" => "alice@example.com"}}
    invalid = %{"user" => %{"username" => "al", "email" => "not-an-email"}}

    # mount: empty local user list, save disabled until the form is valid.
    assert render(socket) =~ "[Local Runtime] User List:"
    refute render(socket) =~ "<li>Username:"

    # validate: a bad form surfaces changeset errors and keeps save disabled.
    {:noreply, invalid_socket} = FormDemoLocal.handle_event("validate", invalid, socket)
    assert invalid_socket.assigns.form.source.errors != []
    assert invalid_socket.assigns.disabled

    # validate: a good form clears errors and enables save.
    {:noreply, valid_socket} = FormDemoLocal.handle_event("validate", valid, socket)
    assert valid_socket.assigns.form.source.errors == []
    refute valid_socket.assigns.disabled

    # save: the user is appended to the local list and rendered. This is the state
    # that gets pushed to the server-side mirror in the browser.
    {:noreply, saved_socket} = FormDemoLocal.handle_event("save", valid, socket)

    assert Enum.map(saved_socket.assigns.users, &{&1.username, &1.email}) == [
             {"alice", "alice@example.com"}
           ]

    assert render(saved_socket) =~ "Username: alice, Email: alice@example.com"
  end

  defp render(socket) do
    socket.assigns
    |> Map.put(:__changed__, nil)
    |> FormDemoLocal.render()
    |> Phoenix.HTML.Safe.to_iodata()
    |> IO.iodata_to_binary()
  end
end
