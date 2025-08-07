defmodule Popcorn.TrackedObject do
  @moduledoc """
  Used to interoperate with JS.

  Tracked objects are registered in the VM to keep a reference to JS value.
  If there is no reference to tracked object, corresponding JS value is removed.

  See `Popcorn.Wasm.run_js/3` for more details.
  """
  @type t :: %__MODULE__{ref: term()}
  defstruct ref: nil
end

defimpl Jason.Encoder, for: Popcorn.TrackedObject do
  def encode(value, opts) when value.ref != nil do
    [key] = :emscripten.get_tracked([value.ref], :key)
    Jason.Encode.map(%{popcorn_ref: key}, opts)
  end
end

defmodule Popcorn.Wasm do
  @moduledoc """
  Functions for JS side communication.
  """
  alias Popcorn.TrackedObject

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

  @typedoc """
  See `handle_message!/2` docs.
  """
  @type handler_result ::
          {:resolve, promise_reply :: term(), result :: term()}
          | {:reject, promise_reply :: term(), result :: term()}
          | term()

  @typedoc """
  See `handle_message!/2` docs.
  """
  @type message_handler :: (wasm_message() -> handler_result())

  @typedoc """
  See `run_js/2` docs.
  """
  @type js_function() :: String.t()

  # TODO: remove args
  @type run_js_opts() :: [{:return, :ref | :value} | {:args, map()}]

  @type run_js_return() :: TrackedObject.t() | term()
  @type result(t) :: {:ok, t} | {:error, term()}

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
      ["_popcorn_dom_event", name, data, custom] ->
        {:wasm_event, String.to_atom(name), data, custom}

      cast_message ->
        {:wasm_cast, cast_message}
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
  Runs JS code in the WASM iframe context. Takes a JS function as a string.
  Returns list of `TrackedObject` (or values directly, see `return`) for each value in an array returned from JS function.

  ## Options
  - `return`: an atom controlling if `run_js` should return tracked objects list or deserialized return value.
    Possible values: `:ref`, `refs`, `:value`, `:values`. Default: `:ref`

  Passing args and returning values introduces overhead related to serializing and deserializing.

  ## JS function
  The JS function takes an object with following keys:
  - `args`: the arguments passed from Elixir and deserialized for JS.
  - `wasm`: the wasm module used underneath. See "Notes" section.
  - `iframeWindow`: original iframe `window` object. See "Notes" section.

  Passed function should return an array of tracked values. Their lifetime is tied to returned `TrackedObject`s refs.
  Value stops being tracked after Elixir VM garbage collects the `TrackedObject`.

  ## Notes
  `wasm` argument passed to JS function is a emscripten's instantiated wasm module.
  It does not have same crash and timeout resiliency as `Popcorn` object. This matters when calling `wasm.call()` which may hang the runtime if used improperly.

  `run_js` executes code in iframe context which normally would use `window` (implicitly and explicitly) from iframe.
  Typically, `window` is used to interact with main browser window context. Popcorn shadows `window` variable for that reason.

  ## Example
  ```
  Popcorn.Wasm.run_js(\"""
  ({ args }) => {
    const n = args.n;
    return [n-1, n, n-1];
  }
  \""", %{n: 5})

  #=> {:ok, [%TrackedObject{}, %TrackedObject{}, %TrackedObject{}]}
  ```
  """
  @spec run_js(js_function()) :: result(run_js_return())
  @spec run_js(js_function(), map()) :: result(run_js_return())
  @spec run_js(js_function(), map(), run_js_opts()) :: result(run_js_return())
  def run_js(function, args \\ %{}, opts \\ []) do
    %{return: return_type} = opts_to_map(opts, return: :ref)

    with {:ok, wrapped_js_fn} <- with_wrapper(function, args),
         {:ok, refs} <- :emscripten.run_script_tracked(wrapped_js_fn) do
      tracked_objects = Enum.map(refs, &%TrackedObject{ref: &1})
      # Lv. 17 magic ahead
      # args _must_ not be GC'd until we execute JS function since it will remove the object from JS side.
      # Call to any external function ensures that reference will outlive JS call and compiler won't optimize it.
      __MODULE__.id(args)

      case {return_type, tracked_objects} do
        {:refs, objs} -> {:ok, objs}
        {:ref, [obj]} -> {:ok, obj}
        {:ref, []} -> {:ok, nil}
        {:values, objs} -> get_tracked_values(objs)
        {:value, [obj]} -> [obj] |> get_tracked_values() |> hd()
        {:value, []} -> {:ok, nil}
      end
    end
  rescue
    e -> {:error, e}
  end

  @spec run_js!(js_function()) :: result(run_js_return())
  @spec run_js!(js_function(), map()) :: result(run_js_return())
  @spec run_js!(js_function(), map(), run_js_opts()) :: result(run_js_return())
  def run_js!(function, args \\ %{}, opts \\ []) do
    {:ok, tracked} = run_js(function, args, opts)
    tracked
  end

  @doc """
  Takes a list of tracked objects and returns a list of terms coresponding to them.

  This function has an overhead coming from the need of serialization and deserialization.
  For variant that assumes success, see `get_tracked_values!/1`.
  """
  @spec get_tracked_values([TrackedObject.t()]) :: [
          {:error, :not_found} | {:error, :unserializable} | {:ok, String.t()}
        ]
  def get_tracked_values(refs) when is_list(refs) do
    refs
    |> Enum.map(fn %TrackedObject{ref: ref} -> ref end)
    |> :emscripten.get_tracked(:value)
    |> Enum.map(fn
      {:error, :badkey} -> {:error, :not_found}
      {:error, :badvalue} -> {:error, :unserializable}
      {:ok, value} -> deserialize(value)
    end)
  end

  @doc """
  Raises on error. See `get_tracked_values/1`.
  """
  @spec get_tracked_values!([TrackedObject.t()]) :: [String.t()]
  def get_tracked_values!(refs) when is_list(refs) do
    refs
    |> get_tracked_values()
    |> Enum.map(fn {:ok, deserialized} -> deserialized end)
  end

  @doc """
  Notifies JS that Elixir side finished initializing. Can be called only once.
  """
  def register(main_process_name) do
    """
    ({ wasm, args }) => {
      wasm.onElixirReady?.(args.main);
    }
    """
    |> run_js(%{main: to_string(main_process_name)})

    :ok
  end

  @doc """
  Registers event listeners for elements matching given `selectors`.
  Events will be sent to the process registered under `ex_receiver` name and contain the used `selector` and `HTMLElement.dataset`.

  To unregister listener, use returned ref with `unregister_event_listeners/1`
  """
  @spec register_event_listeners(String.t(), [String.t()], atom()) :: run_js_return()
  def register_event_listeners(event_name, selectors, ex_receiver) do
    """
    ({ wasm, args }) => {
      const { selectors, event_name, event_receiver } = args;
      const removeListenersFunctions = [];
      selectors.forEach((selector) => {
        const nodes = document.querySelectorAll(selector);
        const fn = (event) => {
          wasm.cast(event_receiver, ["_popcorn_dom_event", event_name, selector, event.target.dataset]);
        };
        nodes.forEach((node) => node.addEventListener(event_name, fn));

        removeListenersFunctions.push(() => {
          nodes.forEach((node) => node.removeEventListener(event_name, fn));
        });
      });

      const key = wasm.nextTrackedObjectKey();
      const cleanupFn = () => {
        removeListenersFunctions.forEach((fn) => fn());
        wasm.cleanupFunctions.delete(key);
      };
      wasm.cleanupFunctions.set(key, cleanupFn);

      return [new TrackedValue({key: key, value: cleanupFn})];
    }
    """
    |> run_js!(
      %{
        selectors: List.wrap(selectors),
        event_name: event_name,
        event_receiver: ex_receiver
      },
      return: :ref
    )
  end

  @doc """
  Unregister event listener. See `register_event_listeners/3`.
  """
  def unregister_event_listeners(ref) do
    """
    ({ args }) => {
      args.cleanupFn();
    }
    """
    |> run_js(%{cleanupFn: ref})
  end

  @doc false
  def id(x), do: x

  defp with_wrapper(js_function, args) do
    with {:ok, serialized_args} <- serialize(args) do
      code = """
      (Module) => {
        const iframeWindow = globalThis.window;
        const window = globalThis.window.parent;
        const document = globalThis.window.parent.document;
        return (#{js_function})({
          wasm: Module,
          args: Module.deserialize(JSON.stringify(#{serialized_args})),
          iframeWindow: iframeWindow
        });
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

  defp opts_to_map(opts, values), do: opts |> Keyword.validate!(values) |> Map.new()
end
