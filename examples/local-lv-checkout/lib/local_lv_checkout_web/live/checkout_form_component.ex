defmodule LocalLvCheckoutWeb.Live.CheckoutFormComponent do
  use LocalLvCheckoutWeb, :live_component

  def render(assigns) do
    ~H"""
    <div>
      <.local_live_view id={@llv_id} view="CheckoutLive" />
    </div>
    """
  end
end
