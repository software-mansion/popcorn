defmodule DemoApp.BehaviourImpl do
  @moduledoc false
  @behaviour DemoApp.Behaviour

  @impl true
  def hello() do
    DemoApp.BehaviourImplDep.print_hello()
    :ok
  end
end
