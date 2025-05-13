defmodule Popcorn.RemoteObject do
  defstruct ref: nil
end

defimpl Jason.Encoder, for: Popcorn.RemoteObject do
  def encode(value, opts) when value.ref != nil do
    key = :emscripten.from_remote_object(value.ref, :key)
    Jason.Encode.map(%{popcorn_ref: key}, opts)
  end
end

defmodule Popcorn.Wasm do
  @moduledoc """
  Functions for interacting with JS side.
  """
  alias Popcorn.RemoteObject

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

  @typedoc """
  See `run_js/2` docs.
  """
  @type js_function() :: String.t()

  @type run_js_opts() :: [{:return, :ref | :value} | {:args, map()}]

  @type run_js_return() :: %RemoteObject{} | term()
  @type result(t) :: {:ok, t} | {:error, term()}

  @type register_event_listener_opts() :: [
          event_keys: [atom()],
          target_node: %RemoteObject{},
          receiver_name: String.t(),
          custom_data: term()
        ]

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
  def parse_message!({:emscripten, {:call, promise, raw_message}}) do
    {:ok, message} = deserialize(raw_message)
    {:wasm_call, message, promise}
  end

  def parse_message!({:emscripten, {:cast, raw_message}}) do
    {:ok, message} = deserialize(raw_message)

    case message do
      ["dom_event", name, data, custom] -> {:wasm_event, String.to_atom(name), data, custom}
      cast_message -> {:wasm_cast, cast_message}
    end
  end

  @spec resolve(term(), promise()) :: :ok
  def resolve(term, promise) do
    with {:ok, raw_message} <- serialize(term) do
      :emscripten.promise_resolve(promise, raw_message)
      :ok
    end
  end

  @spec reject(term(), promise()) :: :ok
  def reject(term, promise) do
    with {:ok, raw_message} <- serialize(term) do
      :emscripten.promise_reject(promise, raw_message)
      :ok
    end
  end

  @doc """
  Runs JS code in browser's main thread in the WASM iframe context. Takes a JS function as string. The JS function takes an object with following keys:
  - `args`: the arguments passed from Elixir and deserialized for JS.
  - `window`: the main window context. Use it instead of calling (implicitly or explicitly) iframe window in the function.

  Value returned from function is saved in JS context. It's lifetime is tied to (possibly) returned RemoteObject's ref.
  If this ref is garbage collected by Elixir VM, value is destroyed in JS.

  `opts`:
  - `args`: a map of serializable Elixir values passed to the function. Default: `%{}`
  - `return`: an atom controlling if `run_js` should return a remote object with reference to JS return value or deserialized return value. Possible values: `:ref`, `:value`. Default: `:ref`

  By default only RemoteObject is returned.
  Passing args and returning value introduces overhead related to serializing and deserializing.
  """
  @spec run_js(js_function(), run_js_opts()) :: result(run_js_return())
  @spec run_js(js_function()) :: result(run_js_return())
  def run_js(function, opts \\ []) do
    %{return: return_type, args: args} = opts_to_map(opts, return: :ref, args: %{})

    with {:ok, wrapped_js_fn} <- with_wrapper(function, args),
         {:ok, ref} <- run_js_fn(wrapped_js_fn) do
      # Lv. 17 magic ahead
      # args _must_ not be GC'd until we execute JS function since it will remove the object from JS side.
      # Call to any external function ensures that reference will outlive JS call and compiler won't optimize it.
      __MODULE__.id(args)

      case return_type do
        :ref -> {:ok, ref}
        :value -> get_remote_object_value(ref)
      end
    end
  rescue
    e -> {:error, e}
  end

  @doc false
  def id(x), do: x

  @doc """
  Raises on error.
  See `run_js/2`.
  """
  @spec run_js!(js_function(), run_js_opts()) :: run_js_return()
  def run_js!(function, opts \\ []) do
    {:ok, value} = run_js(function, opts)
    value
  end

  @doc """
  Returns Elixir term based on RemoteObject.
  """
  @spec get_remote_object_value(%RemoteObject{}) :: {:ok, term()} | {:error, term()}
  def get_remote_object_value(%RemoteObject{ref: ref}) do
    with {:ok, serialized} <- :emscripten.from_remote_object(ref, :value) do
      deserialize(serialized)
    end
  end

  @doc """
  Raises on error. See `get_remote_object_value/1`.
  """
  @spec get_remote_object_value(%RemoteObject{}) :: term()
  def get_remote_object_value!(remote_object) do
    {:ok, value} = get_remote_object_value(remote_object)
    value
  end

  @doc """
  Notifies JS that Elixir side finished initializing. Can be called only once.
  """
  def register(main_process_name) do
    {:ok, _} =
      """
      ({ wasm, args }) => {
        wasm.onElixirReady?.(args.main);
      }
      """
      |> run_js(args: %{main: main_process_name})

    :ok
  end

  @doc """
  Registers event listener for element with given `selector`. Events will be sent to the process registered under `target` name.
  To get event data, specify needed keys in `event_keys` list.

  To unregister listener, use returned ref with `unregister_event_listener/1`
  """
  @spec register_event_listener(atom(), register_event_listener_opts()) ::
          result(run_js_return())
  def register_event_listener(event_name, opts) do
    %{
      event_keys: event_keys,
      target_node: target_node,
      event_receiver: event_receiver,
      custom_data: custom_data
    } =
      opts_to_map(opts, event_keys: [], target_node: nil, event_receiver: nil, custom_data: nil)

    """
    ({ wasm, args, window, key }) => {
      const { event_receiver, event_name, target_node, event_keys, custom_data } = args;
      const document = window.document;

      const getEventData = (event) => {
        const data = {};
        for(const key of event_keys) {
          data[key] = event[key];
        }
        return data;
      };
      const fn = (event) => {
        wasm.cast(event_receiver, ["dom_event", event_name, getEventData(event), custom_data]);
      };
      const node = target_node;
      node.addEventListener(event_name, fn);
      const cleanupFn = () => {
        node.removeEventListener(event_name, fn);
        wasm.cleanupFunctions.delete(key);
      };
      wasm.cleanupFunctions.set(key, cleanupFn);

      return cleanupFn;
    }
    """
    |> run_js(
      args: %{
        event_name: event_name,
        target_node: target_node,
        event_receiver: event_receiver,
        event_keys: event_keys,
        custom_data: custom_data
      }
    )
  end

  def unregister_event_listener(ref) do
    """
    ({ args }) => {
      args.cleanupFn();
    }
    """
    |> run_js(args: %{cleanupFn: ref})
  end

  defp with_wrapper(js_function, args) do
    with {:ok, serialized_args} <- serialize(args) do
      code = """
      (Module, key) => {
        try {
          return (#{js_function})({
            wasm: Module,
            args: Module.deserialize(JSON.stringify(#{serialized_args})),
            window: window.parent,
            key: key
          });
        } catch (e) {
          console.error(e);
        }
      }
      """

      {:ok, code}
    end
  end

  defp deserialize(message) do
    Jason.decode(message)
  end

  defp serialize(term) do
    Jason.encode(term, escape: :javascript_safe)
  end

  defp run_js_fn(code) do
    with {:ok, ref} <- :emscripten.run_remote_object_fn_script(code, main_thread: true) do
      {:ok, %RemoteObject{ref: ref}}
    end
  end

  defp opts_to_map(opts, values), do: opts |> Keyword.validate!(values) |> Map.new()
end
