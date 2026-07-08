defmodule FormDemoWeb.FormDemoSyncTest do
  @moduledoc """
  Covers the server side of the LocalLiveView <-> Mirror synchronization.

  In the browser the offline `FormDemoLocal` view calls `mirror_sync(socket, [:users])`
  on save. That pushes the `:users` assign over the LLV channel, which invokes
  `Mirror.FormDemoLocal.handle_sync/3`, which in turn broadcasts the synced assigns on
  a PubSub topic keyed by the LLV instance id. The online `FormDemoWeb.FormDemoLive`
  subscribes to that topic and mirrors the user list into its "[Server Runtime]" view.

  These tests assert (a) that `handle_sync/3` transmits exactly the synced assigns on
  the expected topic, and (b) that the online LiveView reflects them — i.e. the full
  server-side sync path works without a browser.
  """
  use FormDemoWeb.ConnCase, async: false

  import Phoenix.LiveViewTest

  describe "Mirror.FormDemoLocal.handle_sync/3" do
    test "broadcasts the synced assigns on the LLV mirror topic and echoes them back" do
      llv_id = "form-demo-local-test-#{System.unique_integer([:positive])}"
      Phoenix.PubSub.subscribe(FormDemo.PubSub, "llv_mirror:FormDemoLocal:#{llv_id}")

      users = [%{"username" => "bob", "email" => "bob@example.com"}]
      local_assigns = %{"users" => users}

      # Return value becomes the mirror's stored assigns (conflict-resolution point).
      assert {:ok, ^local_assigns} =
               Mirror.FormDemoLocal.handle_sync(local_assigns, %{}, %{llv_id: llv_id})

      # Exactly the payload the online LiveView listens for.
      assert_receive {:llv_attrs, %{"users" => ^users}}
    end
  end

  describe "FormDemoLive (server runtime)" do
    test "starts with an empty server-side user list", %{conn: conn} do
      {:ok, _view, html} = live_isolated(conn, FormDemoWeb.FormDemoLive)

      assert html =~ "[Server Runtime] User List:"
      refute html =~ "Username:"
    end

    test "renders users synced through the mirror", %{conn: conn} do
      {:ok, view, html} = live_isolated(conn, FormDemoWeb.FormDemoLive)
      llv_id = llv_id_from_html(html)

      user = %{"username" => "alice", "email" => "alice@example.com"}

      # Drive the exact path a browser save triggers: the mirror broadcasts to the
      # topic this LiveView subscribed to on mount.
      assert {:ok, _} =
               Mirror.FormDemoLocal.handle_sync(%{"users" => [user]}, %{}, %{llv_id: llv_id})

      assert render(view) =~ "Username: alice, Email: alice@example.com"
    end
  end

  # The mount-point id encodes the LiveView's socket id; the mirror topic is keyed on it.
  defp llv_id_from_html(html) do
    [_, llv_id] = Regex.run(~r/id="(form-demo-local-[^"]+)"/, html)
    llv_id
  end
end
