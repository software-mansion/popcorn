defmodule App.Fission do
  defguard is_wasm_message(msg) when elem(msg, 0) == :emscripten

  def dispatch_wasm_message({:emscripten, {:call, promise, message}}, state, handler) do
    message
    |> deserialize()
    |> then(&{:wasm_call, &1})
    |> handler.(state)
    |> case do
      {:resolve, reply, new_state} ->
        resolve(reply, promise)
        {reply, new_state}

      {:reject, reply, new_state} ->
        reject(reply, promise)
        {reply, new_state}
    end
  end

  def resolve(term, promise) do
    :emscripten.promise_resolve(promise, serialize(term))
  end

  def reject(term, promise) do
    :emscripten.promise_reject(promise, serialize(term))
  end

  @doc false
  def deserialize(message) do
    String.split(message, ":", parts: 2)
  end

  @doc false
  def serialize(term) do
    inspect(term)
  end
end
