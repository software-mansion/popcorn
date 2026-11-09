defmodule Popcorn.Fetch do
  @moduledoc """
  Sends HTTP requests through the browser's `fetch()` API.

  ## Using Req

  Req is optional. If your application includes Req, Popcorn installs this adapter when it starts inside the Popcorn runtime.
  If it includes Mix, Popcorn installs it once `Mix.install/2` installs Req.
  Popcorn preserves any adapter in Req's `:default_options` configuration.
  The package has the adapter only when it has Req or Mix.
  The adapter supports Req versions from `0.5.0` through `0.8.0-rc.0`.

  You can also select the adapter per request:

  ```elixir
  Req.get!("https://api.example.com/status", adapter: Popcorn.Fetch.adapter())
  ```

  The adapter supports Req's `:into` functions, collectables, and `:self` streams.

  It buffers request bodies before upload.
  The `:receive_timeout` option limits the wait for each response message and defaults to `30_000` ms.
  Adapter failures return `Req.TransportError` exceptions through Req's normal error handling.

  ## Using it without Req

  Use `request/2` for a response map with a binary body. It does not require Req or decode JSON responses.

  ## Browser limits

  - Cross-origin requests need permission from the target server's CORS policy.
  - The browser follows redirects. Req's `:max_redirects` and `:redirect_log_level` options do not control those redirects.
  - The browser decompresses responses. Req's `:raw` option cannot preserve compressed response bytes.
  - The browser controls restricted headers, such as `Host` and `Cookie`.
  """

  alias Popcorn.JS

  require Logger

  @default_timeout 30_000

  @typedoc """
  A request with a method and URL. Headers are string pairs, and the optional body is a binary.
  """
  @type request :: %{
          required(:method) => String.t(),
          required(:url) => String.t(),
          optional(:headers) => [{String.t(), String.t()}],
          optional(:body) => binary() | nil
        }

  @typedoc """
  An HTTP status, browser-visible headers, and a binary response body.
  """
  @type response :: %{
          status: non_neg_integer(),
          headers: [{String.t(), String.t()}],
          body: binary()
        }

  @typedoc """
  A request failure.

  - `:timeout` - the response exceeded the timeout. The adapter aborts the browser request.
  - `{:fetch, message}` - the browser reported a network or fetch failure.
  - `{:bridge, reason}` - JavaScript execution failed. See `Popcorn.JS.run/3`.
  """
  @type error :: :timeout | {:fetch, String.t()} | {:bridge, term()}

  # Fetch runs asynchronously and sends response chunks to `target`.
  @start_js """
  (args, { send }) => {
    const CHUNK = 2 ** 14;
    const ITER_MAX = 1000;

    async function reply(message) {
      return send(args.target, {
        ...message,
        popcorn_fetch: args.popcorn_fetch,
      });
    }

    function toBase64(bytes) {
      return btoa(String.fromCharCode.apply(null, bytes));
    }

    function fromBase64(str) {
      return Uint8Array.from(atob(str), (c) => c.charCodeAt(0));
    }

    async function sendChunks(bytes) {
      for (let i = 0; i < bytes.length; i += CHUNK) {
        await reply({
          event: "chunk",
          data: toBase64(bytes.subarray(i, i + CHUNK)),
        });
      }
    }

    const controller = new AbortController();

    (async () => {
      try {
        const init = {
          method: args.method,
          headers: args.headers,
          signal: controller.signal,
        };
        if (args.body !== undefined) {
          init.body = fromBase64(args.body);
        }

        const response = await fetch(args.url, init);
        await reply({
          event: "status",
          status: response.status,
          headers: [...response.headers],
        });

        if (response.body !== null) {
          const reader = response.body.getReader();
          for (let i = 0; i < ITER_MAX; i++) {
            const { done, value } = await reader.read();
            if (done) break;
            await sendChunks(value);
          }
        }
        await reply({ event: "done" });
      } catch (error) {
        await reply({ event: "error", error: error.toString() });
      }
    })();

    return new TrackedValue(controller, () => controller.abort());
  }
  """

  @abort_js "({ controller }) => { controller.abort(); }"

  @doc """
  Sends an HTTP request and returns the complete response.

  HTTP error statuses, such as 404, still return `{:ok, response}`.
  Transport failures return `{:error, reason}`. See `t:error/0`.

  ## Options

  - `:timeout` - the total response timeout in milliseconds, or `:infinity`. Defaults to `#{@default_timeout}`. The adapter aborts the request on timeout.

  ## Example

  ```elixir
  {:ok, response} = Popcorn.Fetch.request(%{method: "GET", url: "/api/status"})
  response.body
  #=> ~s({"status":"ready"})
  ```
  """
  @spec request(request(), [{:timeout, timeout()}]) :: {:ok, response()} | {:error, error()}
  def request(req, opts \\ []) when is_map(req) and is_list(opts) do
    headers = Enum.map(Map.get(req, :headers, []), fn {name, value} -> [name, value] end)
    req = req |> Map.take([:method, :url, :body]) |> Map.put(:headers, headers)

    timeout = Keyword.get(opts, :timeout, @default_timeout)
    wait = {:total, deadline(timeout)}

    with {:ok, handle} <- start(req, self()),
         {:ok, status, headers} <- collect_head(handle, wait),
         {:ok, body} <- collect_body(handle, wait) do
      {:ok, %{status: status, headers: headers, body: body}}
    end
  end

  # The adapter's implementation is in Popcorn.Fetch.ReqAdapter, which the
  # packager leaves out of packages that have neither Req nor Mix
  @compile {:no_warn_undefined, [Req.Adapter, Popcorn.Fetch.ReqAdapter]}

  if Code.ensure_loaded?(Req.Adapter) do
    @behaviour Req.Adapter
  end

  @doc "Returns the adapter value for the installed Req version."
  def adapter do
    version = :req |> Application.spec(:vsn) |> to_string()

    if Version.match?(version, ">= 0.7.0") do
      __MODULE__
    else
      &__MODULE__.run/1
    end
  end

  @doc false
  defdelegate run(request), to: Popcorn.Fetch.ReqAdapter

  if Code.ensure_loaded?(Req.Adapter) do
    @impl Req.Adapter
  end

  @doc false
  defdelegate stream(request, acc, fun, state), to: Popcorn.Fetch.ReqAdapter

  # At boot, and after Mix.install/2, which can install Req, see
  # patches/mix/Elixir.Mix.erl. It keeps any adapter in Req's default options.
  @doc false
  @spec install_req_adapter() :: :ok
  def install_req_adapter do
    with true <- JS.available?(),
         true <- Code.ensure_loaded?(Req),
         true <- Code.ensure_loaded?(Popcorn.Fetch.ReqAdapter),
         opts = Application.get_env(:req, :default_options, []),
         false <- Keyword.has_key?(opts, :adapter) do
      Application.put_env(:req, :default_options, Keyword.put(opts, :adapter, adapter()))
      Logger.debug("using Popcorn.Fetch as Req's adapter")
    end

    :ok
  end

  # The bridge to the browser's fetch(), which Popcorn.Fetch.ReqAdapter uses too

  @doc false
  def default_timeout, do: @default_timeout

  @doc false
  def body_stream(handle, wait) do
    Stream.resource(
      fn -> %{handle: handle, wait: wait, completed: false} end,
      fn state ->
        case recv(state.handle, state.wait) do
          {:chunk, data} -> {[{:data, data}], state}
          :done -> {:halt, %{state | completed: true}}
          {:error, reason} -> {[{:error, reason}], state}
        end
      end,
      fn
        %{completed: true} -> :ok
        state -> cancel(state.handle)
      end
    )
  end

  @doc false
  def start(req, target) do
    id = System.unique_integer([:positive])

    args =
      %{
        popcorn_fetch: id,
        target: target,
        method: req.method,
        url: req.url,
        headers: Map.get(req, :headers, [])
      }
      |> put_body(Map.get(req, :body))

    case JS.run(@start_js, args) do
      {:ok, controller} -> {:ok, %{id: id, target: target, controller: controller}}
      {:error, reason} -> {:error, {:bridge, reason}}
    end
  end

  defp put_body(args, nil), do: args
  defp put_body(args, body), do: Map.put(args, :body, Base.encode64(body))

  @doc false
  def collect_head(handle, wait) do
    case recv(handle, wait) do
      {:head, status, headers} ->
        {:ok, status, headers}

      {:error, reason} ->
        cancel(handle)
        {:error, reason}
    end
  end

  @doc false
  def collect_body(handle, wait) do
    result =
      handle
      |> body_stream(wait)
      |> Enum.reduce_while({:ok, []}, fn
        {:data, data}, {:ok, acc} -> {:cont, {:ok, [data | acc]}}
        {:error, reason}, {:ok, _acc} -> {:halt, {:error, reason}}
      end)

    case result do
      {:ok, chunks} -> {:ok, chunks |> Enum.reverse() |> IO.iodata_to_binary()}
      {:error, reason} -> {:error, reason}
    end
  end

  defp recv(handle, wait) do
    id = handle.id

    receive do
      {:wasm, %{"popcorn_fetch" => ^id} = message} -> decode(message)
    after
      wait_timeout(wait) -> {:error, :timeout}
    end
  end

  @doc false
  def decode(%{"event" => "status", "status" => status, "headers" => headers}) do
    {:head, status, Enum.map(headers, fn [name, value] -> {name, value} end)}
  end

  def decode(%{"event" => "chunk", "data" => data}), do: {:chunk, Base.decode64!(data)}
  def decode(%{"event" => "done"}), do: :done
  def decode(%{"event" => "error", "error" => message}), do: {:error, {:fetch, hint(message)}}

  # `TypeError: Failed to fetch` is what the browser reports for a CORS
  # rejection and for a genuine network failure alike; it never says which.
  defp hint("TypeError: Failed to fetch" = message) do
    """
    #{message}

    The request was blocked. A missing CORS response header on the target is the
    most common cause; a network failure is the other.
    """
  end

  defp hint(message), do: message

  @doc false
  def cancel(handle) do
    JS.run(@abort_js, %{controller: handle.controller})
    flush_bridge(handle.id)
    :ok
  end

  defp flush_bridge(id) do
    receive do
      {:wasm, %{"popcorn_fetch" => ^id}} -> flush_bridge(id)
    after
      0 -> :ok
    end
  end

  defp deadline(:infinity), do: :infinity
  defp deadline(timeout), do: System.monotonic_time(:millisecond) + timeout

  defp wait_timeout({:each, timeout}), do: timeout
  defp wait_timeout({:total, :infinity}), do: :infinity

  defp wait_timeout({:total, deadline}) do
    max(0, deadline - System.monotonic_time(:millisecond))
  end
end
