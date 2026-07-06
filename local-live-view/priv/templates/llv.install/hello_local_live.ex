use <%= @web_module %>, :live_view

@impl true
def render(assigns) do
  ~H"""
  <.local_live_view view="HelloLocal" />
  """
end
