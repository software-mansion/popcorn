defmodule LocalLiveView.Utils do
  @moduledoc false

  @doc """
  Recursively turns a term into something JSON-serializable to cross the WASM
  boundary: structs become maps, map keys become strings, lists are mapped
  through. Used when sending assigns/payloads from the runtime to JS.
  """
  def to_serializable(value) when is_struct(value),
    do: value |> Map.from_struct() |> to_serializable()

  def to_serializable(value) when is_map(value),
    do: Map.new(value, fn {k, v} -> {to_string(k), to_serializable(v)} end)

  def to_serializable(value) when is_list(value),
    do: Enum.map(value, &to_serializable/1)

  def to_serializable(value), do: value
end
