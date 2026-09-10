defmodule DemoApp.OnLoadDep do
  @moduledoc false
  # Only reachable from DemoApp.OnLoad's on_load function.

  def touch(), do: :ok
end
