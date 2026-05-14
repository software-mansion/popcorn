defmodule DemoApp.AnotherDead do
  @moduledoc false
  # Never referenced anywhere — dead module.
  def hello, do: "world"
  def goodbye, do: "farewell"
end
