defmodule FormDemoLocal.Mirror do
  def handle_sync(%{"users" => _} = payload, _attrs) do
    Phoenix.PubSub.broadcast(FormDemo.PubSub, "llv_mirror:FormDemoLocal", {:llv_attrs, payload})
    {:ok, payload}
  end
end
