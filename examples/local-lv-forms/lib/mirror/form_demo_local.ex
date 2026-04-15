defmodule Mirror.FormDemoLocal do
  use LocalLiveView.Mirror

  @impl true
  def handle_sync(%{"users" => _} = local_assigns, _mirror_assigns) do
    Phoenix.PubSub.broadcast(FormDemo.PubSub, "llv_mirror:FormDemoLocal", {:llv_attrs, local_assigns})
    {:ok, local_assigns}
  end
end
