defmodule DemoApp.Worker do
  @moduledoc false
  # Called from Application.start/2 — reachable.
  def process(input) do
    input |> upcase() |> wrap()
  end

  defp upcase(s), do: String.upcase(s)
  defp wrap(s), do: "[#{s}]"

  def unused(foo) do
    foo + 1
  end
end
