defmodule FormDemoLocalAtomVMTest do
  @moduledoc """
  Runs the offline (LocalLiveView) form on a real native AtomVM, the same runtime
  that powers the example in the browser. We compile a quoted expression that mounts
  the view, runs the validate/save events and renders the resulting states to HTML,
  run it on the AtomVM binary, and assert on the assigns/HTML it produces.

  `save` calls `LocalLiveView.mirror_sync/2`, which in the browser pushes the synced
  assigns to the server-side mirror. On the native runtime the underlying `run_js`
  bridge is a no-op (it returns an error that `mirror_sync` ignores), so the local
  state transitions still run exactly as they do in the browser.

  Build the runtime once with:

      MIX_TARGET=wasm mix popcorn.build_runtime --target unix --out-dir test/popcorn_runtime_source
  """
  use ExUnit.Case
  alias Popcorn.Support.AtomVM

  @moduletag :tmp_dir
  @moduletag timeout: :timer.minutes(3)

  test "FormDemoLocal mount/validate/save/render runs on AtomVM", %{tmp_dir: dir} do
    bundle =
      quote do
        render = fn socket ->
          socket.assigns
          |> Map.put(:__changed__, nil)
          |> FormDemoLocal.render()
          |> Phoenix.HTML.Safe.to_iodata()
          |> IO.iodata_to_binary()
        end

        {:ok, socket} = FormDemoLocal.mount(%{}, %{}, %Phoenix.LiveView.Socket{})
        initial_html = render.(socket)

        # Events arrive form-wrapped, exactly as Phoenix delivers phx-change/phx-submit.
        valid = %{"user" => %{"username" => "alice", "email" => "alice@example.com"}}
        invalid = %{"user" => %{"username" => "al", "email" => "not-an-email"}}

        {:noreply, invalid_socket} = FormDemoLocal.handle_event("validate", invalid, socket)
        {:noreply, valid_socket} = FormDemoLocal.handle_event("validate", valid, socket)

        {:noreply, saved_socket} = FormDemoLocal.handle_event("save", valid, socket)
        saved_html = render.(saved_socket)

        %{
          initial_html: initial_html,
          invalid_error_count: length(invalid_socket.assigns.form.source.errors),
          invalid_disabled: invalid_socket.assigns.disabled,
          valid_error_count: length(valid_socket.assigns.form.source.errors),
          valid_disabled: valid_socket.assigns.disabled,
          saved_users:
            Enum.map(saved_socket.assigns.users, fn u ->
              %{"username" => u.username, "email" => u.email}
            end),
          saved_html: saved_html
        }
      end
      |> AtomVM.compile_quoted()

    result = AtomVM.run(bundle, dir)

    # mount: empty local user list, save disabled until the form is valid.
    assert result.initial_html =~ "[Local Runtime] User List:"
    refute result.initial_html =~ "<li>Username:"

    # validate: a bad form surfaces changeset errors and keeps save disabled.
    assert result.invalid_error_count > 0
    assert result.invalid_disabled

    # validate: a good form clears errors and enables save.
    assert result.valid_error_count == 0
    refute result.valid_disabled

    # save: the user is appended to the local list and rendered. This is the state
    # that gets pushed to the server-side mirror in the browser.
    assert result.saved_users == [%{"username" => "alice", "email" => "alice@example.com"}]
    assert result.saved_html =~ "Username: alice, Email: alice@example.com"
  end
end
