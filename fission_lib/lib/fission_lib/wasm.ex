defmodule FissionLib.Wasm do
  @moduledoc """
  Functions for working with messages received from JS side.
  """
  defguardp is_tagged_emscripten(msg) when elem(msg, 0) == :emscripten
  defguardp is_call(msg) when elem(elem(msg, 1), 0) == :call and tuple_size(elem(msg, 1)) == 3
  defguardp is_cast(msg) when elem(elem(msg, 1), 0) == :cast and tuple_size(elem(msg, 1)) == 2

  defguard is_wasm_message(msg) when is_tagged_emscripten(msg) and (is_call(msg) or is_cast(msg))

  @type promise :: term()

  @typedoc """
  Message received from JS side with binary data.
  """
  @type raw_message ::
          {:emscripten, {:call, promise(), data :: binary()}}
          | {:emscripten, {:cast, data :: binary()}}

  @typedoc """
  Parsed raw message with data transformed to terms.
  """
  @type wasm_message ::
          {:wasm_call, data :: term(), promise()}
          | {:wasm_cast, data :: term()}

  @type handler_result ::
          {:resolve, promise_reply :: term(), result :: term()}
          | {:reject, promise_reply :: term(), result :: term()}
          | term()

  @type message_handler :: (wasm_message() -> handler_result())

  @doc """
  Deserializes raw message and calls handler with it. If the message was a :wasm_call, settles the promise with a value.
  Returns handler result.
  """
  @spec handle_message!(raw_message(), message_handler()) :: term()
  def handle_message!(raw_msg, handler) when is_wasm_message(raw_msg) do
    case parse_message!(raw_msg) do
      {:wasm_call, message, promise} ->
        {promise_state, promise_reply, result} = handler.({:wasm_call, message})

        case promise_state do
          :resolve -> resolve(promise_reply, promise)
          :reject -> reject(promise_reply, promise)
        end

        result

      message ->
        handler.(message)
    end
  end

  @doc """
  Deserializes message received from JS side.
  """
  @spec parse_message!(raw_message()) :: wasm_message()
  def parse_message!({:emscripten, {:call, promise, message}}) do
    {:wasm_call, deserialize(message), promise}
  end

  def parse_message!({:emscripten, {:cast, message}}) do
    {:wasm_cast, deserialize(message)}
  end

  @spec resolve(term(), promise()) :: :ok
  def resolve(term, promise) do
    :emscripten.promise_resolve(promise, serialize(term))
    :ok
  end

  @spec reject(term(), promise()) :: :ok
  def reject(term, promise) do
    :emscripten.promise_reject(promise, serialize(term))
    :ok
  end

  defp deserialize(message) do
    String.split(message, ":", parts: 2)
  end

  defp serialize(term) do
    inspect(term)
  end
end
