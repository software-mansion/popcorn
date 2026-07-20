defmodule Popcorn.Wasm.Error do
  @moduledoc """
  Raised by `Popcorn.Wasm.run_js!/3` when JS fails.

  `reason` holds the same value `Popcorn.Wasm.run_js/3` would have returned in
  an `{:error, reason}` tuple, so you can match on it after rescuing:

  ```elixir
  try do
    Popcorn.Wasm.run_js!("() => document.title", %{})
  rescue
    e in Popcorn.Wasm.Error ->
      case e.reason do
        :timeout -> retry()
        {:js, _reason} -> give_up()
      end
  end
  ```
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
  Allows for communication from Elixir back to JS.

  Elixir code running through Popcorn runs in OTP/BEAM runtime (in Wasm).
  It will receive events put to the process' mailbox from JS and can use functions
  from this module to talk back.

  ## Receiving messages

  Messages from JS have `{:wasm, payload}` shape. `payload` is fully controlled by user.

  You can use `is_message/1` guard for filtering this type of messages.

  ## Running JavaScript

  `run_js/3` takes a JavaScript function written as a string and a map of
  arguments for it:
  ```elixir
  Popcorn.Wasm.run_js("({ args }) => args.n + 1", %{n: 1})
  #=> {:ok, 2}
  ```

  ## Outside the browser

  Functions in this module call `:wasm` which is our patched-in module for OTP/BEAM.
  You can use `available?/0` to conditionally disable parts of your code when you
  run it on non-Popcorn OTP/BEAM (for example in testing).
  """

  # The runtime provides `:wasm`, so the compiler never sees it. Silencing the
  # warning here means code calling this module compiles without doing the same.
  @compile {:no_warn_undefined, :wasm}

  @typedoc """
  A handle to a JavaScript value that the runtime keeps alive.

  The JavaScript value is released once this term is garbage collected.
  The exact timing isn't guaranteed.

  If you need exact timing, use `TrackedValue` constructor with idempotent
  cleanup function you can call yourself.
  """
  @opaque tracked_value :: {:wasm_tracked_value, reference()}

  @typedoc """
  A message sent from JavaScript to a process.

  The payload is already decoded into Elixir terms. Only value serializable to JSON are supported.
  TrackedValues and PID handles are special-cased in communication.
  """
  @type message :: {:wasm, payload :: term()}

  @type run_js_opts :: [{:timeout, timeout()}]

  @doc """
  Matches a `t:message/0` sent from JavaScript.
  """
  defguard is_message(message)
           when is_tuple(message) and tuple_size(message) == 2 and elem(message, 0) == :wasm

  @doc """
  Returns true when running inside the Popcorn OTP runtime.

  ## Example

  ```elixir
  # in application.ex
  children =
    if Popcorn.Wasm.available?() do
      [MyApp.UI | core_children]
    else
      core_children
    end
  ```
  """
  @spec available?() :: boolean()
  def available? do
    Code.ensure_loaded?(:wasm) and function_exported?(:wasm, :run_js, 3)
  end

  @doc """
  Runs JavaScript synchrounously on the page and returns a handle to JS result.

  `code` is a JavaScript function written as a string. The function takes a `{args, send}` JS object.
  <!-- TODO: split args from the rest of args, `(args, {send}) => { ... }` -->

  - `args` are arguments passed from elixir in a map (types from Elixir are converted to JS ones).
  - `send` is used to send events from JS running in `run_js/3`, same as `popcorn.send()` JS API.

  `run_js/3` returns result converted to Elixir types. You can use `new TrackedValue(value, cleanup_fn)` (`t:tracked_value/0`) to return a handle.
  Underlying TrackedValue is kept on JS side until OTP/BEAM drops all references to a handle.
  If that happens, `cleanup_fn` is called in JS and value is removed from JS.
  <!-- TODO: show explicit serialization errors -->

  ## Options

  - `:timeout` - how long to wait for a reply, in milliseconds. Defaults to `5_000`.

  ## Examples

  ```elixir
  Popcorn.Wasm.run_js("({ args }) => args.n + 1", %{n: 1})
  #=> {:ok, 2}
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
  See `run_js/3`. May raise `Popcorn.Wasm.Error`.
  """
  @spec run_js!(String.t(), map(), run_js_opts()) :: term()
  def run_js!(code, args \\ %{}, opts \\ []) when is_binary(code) and is_map(args) do
    case run_js(code, args, opts) do
      {:ok, value} -> value
      {:error, reason} -> raise Popcorn.Wasm.Error, reason: reason
    end
  end

  @doc """
  Sends a message to JavaScript. Semantics follow OTP/BEAM ones: no receive confirmation, returns immediately.
  """
  @spec send(term()) :: :ok
  def send(message) do
    bridge().send(message)
  end

  defp bridge, do: Application.get_env(:popcorn_otp, :wasm_bridge, :wasm)
end
