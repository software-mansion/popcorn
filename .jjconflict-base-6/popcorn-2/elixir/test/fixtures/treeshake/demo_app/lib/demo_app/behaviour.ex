defmodule DemoApp.Behaviour do
  @moduledoc false
  @callback hello() :: :ok

  def call_hello(module) do
    module.hello()
  end
end
