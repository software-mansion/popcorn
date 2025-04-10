defmodule App.Fission.JsServer do
  @callback init(init_arg :: term) :: {:ok, state} when state: any

  @callback handle_js_call(request :: term, promise :: term, state :: term) ::
              {:resolve, reply, new_state}
              | {:reject, reply, new_state}
              | {:noreply, new_state}
              | {:stop, reason, reply, new_state}
              | {:stop, reason, new_state}
            when reply: term, new_state: term, reason: term

  @callback handle_js_cast(request :: term, state :: term) ::
              {:noreply, new_state}
              | {:stop, reason :: term, new_state}
            when new_state: term

  @optional_callbacks handle_js_cast: 2,
                      handle_js_call: 3

  @type name :: atom | {:global, term} | {:via, module, term}
  @type option :: {:name, name} | {:timeout, timeout}

  defmacro __using__(_opts) do
    alias App.Fission.JsServer

    quote location: :keep do
      @behaviour GenServer

      def handle_info({:emscripten, {:call, promise, message}}, state) do
        message
        |> JsServer.deserialize()
        |> handle_js_call(promise, state)
        |> case do
          {:resolve, reply, new_state} ->
            JsServer.resolve(reply, promise)
            {:noreply, new_state}

          {:reject, reply, new_state} ->
            JsServer.reject(reply, promise)
            {:noreply, new_state}

          {:noreply, new_state} ->
            {:noreply, new_state}

          {:stop, reason, reply, new_state} ->
            JsServer.reject(reply, promise)
            {:stop, reason, new_state}

          {:stop, reason, new_state} ->
            {:stop, reason, new_state}
        end
      end

      @doc false
      def handle_js_call(msg, _promise, state) do
        {:stop, :not_implemented, state}
      end

      @doc false
      def handle_js_cast(msg, state) do
        {:stop, :not_implemented, state}
      end

      defoverridable handle_js_cast: 2, handle_js_call: 3
    end
  end

  def resolve(term, promise) do
    apply(:emscripten, :promise_resolve, [promise, serialize(term)])
  end

  def reject(term, promise) do
    apply(:emscripten, :promise_reject, [promise, serialize(term)])
  end

  @doc false
  def deserialize(message) do
    message
    |> String.split(":", parts: 2)
    |> List.to_tuple()
  end

  @doc false
  def serialize(term) do
    :io_lib.format(~c"~p", [term])
  end
end
