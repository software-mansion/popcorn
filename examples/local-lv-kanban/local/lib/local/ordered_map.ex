defmodule Local.OrderedMap do
  @moduledoc """
  A map that remembers the order of its keys.

  Backed by a plain map for O(1) lookups plus an explicit list of keys that
  defines iteration order. Insertion appends; `insert_before/4` splices a key
  into an arbitrary position.
  """

  @behaviour Access

  defstruct map: %{}, keys: []

  @type key :: term()
  @type t :: %__MODULE__{map: %{optional(key) => term()}, keys: [key]}

  # Access behaviour — enables `om[key]`, `get_in/2`, `update_in/3`, `pop_in/2`.

  @impl Access
  def fetch(%__MODULE__{map: map}, key), do: Map.fetch(map, key)

  @impl Access
  def get_and_update(%__MODULE__{} = om, key, fun) do
    case fun.(get(om, key)) do
      {current, updated} -> {current, put(om, key, updated)}
      :pop -> pop(om, key)
    end
  end

  @impl Access
  def pop(%__MODULE__{} = om, key) do
    case fetch(om, key) do
      {:ok, value} -> {value, delete(om, key)}
      :error -> {nil, om}
    end
  end

  @doc "An empty ordered map."
  def new, do: %__MODULE__{}

  @doc "Builds an ordered map from a list of `{key, value}` entries, in order."
  def new(entries) do
    Enum.reduce(entries, new(), fn {key, value}, om -> put(om, key, value) end)
  end

  @doc "Builds an ordered map from `values`, deriving each key with `key_fun`."
  def new(values, key_fun) when is_function(key_fun, 1) do
    new(Enum.map(values, fn value -> {key_fun.(value), value} end))
  end

  def get(%__MODULE__{map: map}, key, default \\ nil), do: Map.get(map, key, default)

  def has_key?(%__MODULE__{map: map}, key), do: Map.has_key?(map, key)

  def size(%__MODULE__{keys: keys}), do: length(keys)

  def empty?(%__MODULE__{keys: keys}), do: keys == []

  def keys(%__MODULE__{keys: keys}), do: keys

  @doc "Values in key order."
  def values(%__MODULE__{map: map, keys: keys}), do: Enum.map(keys, &Map.fetch!(map, &1))

  @doc "`{key, value}` pairs in key order."
  def to_list(%__MODULE__{map: map, keys: keys}), do: Enum.map(keys, &{&1, Map.fetch!(map, &1)})

  @doc "Appends a new key, or updates an existing one in place."
  def put(%__MODULE__{map: map, keys: keys} = om, key, value) do
    keys = if Map.has_key?(map, key), do: keys, else: keys ++ [key]
    %{om | map: Map.put(map, key, value), keys: keys}
  end

  def delete(%__MODULE__{map: map, keys: keys} = om, key) do
    %{om | map: Map.delete(map, key), keys: List.delete(keys, key)}
  end

  @doc "Applies `fun` to the value at `key`, which must be present."
  def update!(%__MODULE__{map: map} = om, key, fun) do
    %{om | map: Map.update!(map, key, fun)}
  end

  @doc """
  Inserts (or moves) `key` so it sits immediately before `before_key`.

  A `before_key` of `nil`, or one not present, puts the key at the end.
  """
  def insert_before(%__MODULE__{} = om, key, value, before_key) do
    %__MODULE__{map: map, keys: keys} = delete(om, key)

    keys =
      case before_key && Enum.find_index(keys, &(&1 == before_key)) do
        nil -> keys ++ [key]
        index -> List.insert_at(keys, index, key)
      end

    %__MODULE__{map: Map.put(map, key, value), keys: keys}
  end

  @doc "The key following `key` in order, or `nil` when it is last or absent."
  def next_key(%__MODULE__{keys: keys}, key) do
    case Enum.find_index(keys, &(&1 == key)) do
      nil -> nil
      index -> Enum.at(keys, index + 1)
    end
  end
end
