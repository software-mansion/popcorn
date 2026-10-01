defmodule LlvIntegrationWeb.Layouts do
  use LlvIntegrationWeb, :html

  def root(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta name="csrf-token" content={get_csrf_token()} />
        <title>LocalLiveView integration</title>
        <script defer type="module" src="/assets/js/app.js">
        </script>
      </head>
      <body>
        <.local_live_view
          :if={@conn.assigns[:layout_patcher]}
          view="Patcher"
          id="layout-patcher"
          to={@conn.request_path <> "?tab=patcher"}
        />
        <.link
          :if={@conn.assigns[:layout_patcher]}
          id="layout-link"
          patch={@conn.request_path <> "?tab=layout-link"}
        >
          layout patch link
        </.link>
        {if @conn.assigns[:layout_live],
          do: live_render(@conn, LlvIntegrationWeb.LayoutLive, id: "layout-live")}
        {@inner_content}
      </body>
    </html>
    """
  end
end
