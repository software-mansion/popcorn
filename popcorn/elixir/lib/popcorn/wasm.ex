defmodule Popcorn.Wasm.Error do
  @moduledoc """
  Raised by `Popcorn.Wasm.run_js!/3` when JavaScript fails or the reply times out.

  The `:reason` field contains the reason from the `{:error, reason}` result of `Popcorn.Wasm.run_js/3`.
  """

  @type t :: %__MODULE__{reason: :timeout | {:js, term()}}

  defexception [:reason]

  @impl true
  def message(%__MODULE__{reason: :timeout}), do: "JS didn't reply in time"

  def message(%__MODULE__{reason: {:js, reason}}), do: "JS raised: '#{format(reason)}'"

  defp format(reason) when is_binary(reason), do: reason
  defp format(reason), do: inspect(reason)
end

defmodule Popcorn.Wasm do
  @moduledoc """
  Connects Elixir processes to JavaScript in the browser page.

  ## Receiving messages

  JavaScript calls to `popcorn.send(target, payload)` deliver `{:wasm, payload}` to the target process.
  Use `is_message/1` in guards to match for it.
  Use `Popcorn.Proxy` for GenServer calls and casts.

  ## Values

  From JS side:
  - strings become binaries.
  - arrays become lists.
  - plain objects become maps with string keys.
  - `atom()` and `tuple()` helpers send atoms and tuples. Atoms must already exist in the VM.

  From VM side:
  - Tuples become arrays.
  - Most atoms become strings, with exception of `true`, `false` and `nil` (mapped to `null`).
  - PID handles refer to processes in the VM.

  ## Outside the browser

  Use `available?/0` to check if your code is running natively or in the browser.
  """

  # The runtime provides `:wasm`, so the compiler never sees it. Silencing the
  # warning here means code calling this module compiles without doing the same.
  @compile {:no_warn_undefined, :wasm}

  @typedoc """
  An opaque handle that keeps a JavaScript value alive.

  Return `new TrackedValue(value, cleanup)` from JavaScript function to create a handle.
  Pass the handle in `run_js/3` arguments to access the original value.

  The runtime calls `cleanup` function after BEAM garbage collection releases the handle, or when the VM stops.

  Note:
  Garbage collection does not guarantee prompt cleanup.
  For time-sensitive resources, call an idempotent cleanup function explicitly. This ensures you can call it yourself or it can be called by Popcorn.
  """
  @opaque tracked_value :: {:wasm_tracked_value, reference()}

  @typedoc """
  A JavaScript message with a payload decoded into Elixir terms.
  """
  @type message :: {:wasm, payload :: term()}

  @type run_js_opts :: [{:timeout, timeout()}]

  @doc """
  Matches a `t:message/0` sent from JavaScript.
  """
  defguard is_message(message)
           when is_tuple(message) and tuple_size(message) == 2 and elem(message, 0) == :wasm

  @doc """
  Returns `true` if running in the browser.
  """
  @spec available?() :: boolean()
  def available? do
    Code.ensure_loaded?(:wasm) and function_exported?(:wasm, :run_js, 3)
  end

  @doc """
  Runs a JavaScript function on the page and waits for its result.

  `code` defines a function with the signature `(args, {send, call, cast}) => result`.
  The bridge converts the `args` map to JavaScript values and awaits any returned promise.
  It returns `{:ok, value}` with the result converted to Elixir terms or `{:error, {:js, reason}}`.
  A timeout returns `{:error, :timeout}` and does not cancel JavaScript execution.

  The page's Content Security Policy must permit JavaScript evaluation with `unsafe-eval`.

  ## JS

  The `send` helper sends a message to a BEAM process.
  The `call` and `cast` helpers use `Popcorn.Proxy` to contact GenServers.
  These helpers return promises with the same result objects as their [JavaScript API](JS.Popcorn.html) counterparts.

  Notes:
  - Calls during application startup run before `popcorn.boot()` resolves.
  - Do not await a `call` to the process that executes `run_js/3`. It will cause deadlocks.

  ## Options

  - `:timeout` - the reply timeout, or `:infinity`. Defaults to `5_000` ms.

  ## Examples

  ```elixir
  Popcorn.Wasm.run_js("({n}) => n + 1", %{n: 1})
  #=> {:ok, 2}
  ```

  Use a `t:tracked_value/0` for values such as DOM elements:

  ```elixir
  {:ok, element} = Popcorn.Wasm.run_js("() => new TrackedValue(document.body)")
  Popcorn.Wasm.run_js!("({element}) => { element.textContent = 'Ready'; }", %{element: element})
  ```
  """
  @spec run_js(String.t(), map(), run_js_opts()) ::
          {:ok, term()} | {:error, :timeout | {:js, term()}}
  def run_js(code, args \\ %{}, opts \\ []) when is_binary(code) and is_map(args) do
    {:ok, bridge().run_js(code, args, opts)}
  catch
    # Bridge own errors. `rescue` normalizes into structs which we *don't* want.
    :error, :run_js_timeout -> {:error, :timeout}
    :error, {:run_js, reason} -> {:error, {:js, reason}}
  end

  @doc """
  See run_js/3.
  """
  @spec run_js!(String.t(), map(), run_js_opts()) :: term()
  def run_js!(code, args \\ %{}, opts \\ []) when is_binary(code) and is_map(args) do
    case run_js(code, args, opts) do
      {:ok, value} -> value
      {:error, reason} -> raise Popcorn.Wasm.Error, reason: reason
    end
  end

  @doc """
  Sends a message to the JavaScript callbacks registered with `popcorn.onEvent()`.
  """
  @spec send(term()) :: :ok
  def send(message) do
    bridge().send(message)
  end

  defp bridge, do: Application.get_env(:popcorn, :wasm_bridge, :wasm)
end
