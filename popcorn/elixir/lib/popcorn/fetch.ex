defmodule Popcorn.Fetch do
  @moduledoc """
  Sends HTTP requests through the browser's `fetch()` API.

  ## Using Req

  Req is optional. The adapter is compiled only when Req is available.
  If your application includes Req, Popcorn installs this adapter when it starts inside the Popcorn runtime.
  If you add Req later, recompile Popcorn with `mix deps.compile popcorn --force`.
  Popcorn preserves any adapter in Req's `:default_options` configuration.
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
  - The browser decompresses responses. Responses leave out the `content-encoding` and `content-length` headers of compressed bodies, and Req's `:raw` option cannot preserve compressed response bytes.
  - The browser controls restricted headers, such as `Host` and `Cookie`.
  """

  alias Popcorn.JS

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
          while (true) {
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
    with {:ok, conn} <- start(req, opts),
         {:head, status, headers} <- next(conn, :infinity),
         {:ok, body} <- read_body(conn, :infinity) do
      {:ok, %{status: status, headers: headers, body: body}}
    end
  end

  # The internal API, which request/2, the Req adapter and Popcorn.Fetch.HTTPC are
  # built on. start/2 starts a request, next/2 reads its events and close/1 stops
  # it early. The request's events go to its target process, so only the target
  # can read them.

  # Starts a request, whose events go to `:target`, the caller by default. The
  # `:timeout` bounds the whole response.
  @doc false
  def start(req, opts) when is_map(req) and is_list(opts) do
    id = System.unique_integer([:positive])
    headers = Enum.map(Map.get(req, :headers, []), fn {name, value} -> [name, value] end)

    args =
      %{
        popcorn_fetch: id,
        target: Keyword.get(opts, :target, self()),
        method: req.method,
        url: req.url,
        headers: headers
      }
      |> put_body(Map.get(req, :body))

    case JS.run(@start_js, args) do
      {:ok, controller} ->
        deadline = deadline(Keyword.get(opts, :timeout, @default_timeout))
        {:ok, %{id: id, controller: controller, deadline: deadline}}

      {:error, reason} ->
        {:error, {:bridge, reason}}
    end
  end

  # The request's next event: `{:head, status, headers}`, then `{:data, chunk}` for
  # each chunk of the body, until `:done` or `{:error, reason}`, which end the
  # request. The `timeout` bounds the wait for this event, within the request's.
  @doc false
  def next(conn, timeout) do
    id = conn.id

    receive do
      {:wasm, %{"popcorn_fetch" => ^id} = message} -> decode(message)
    after
      wait(conn.deadline, timeout) ->
        close(conn)
        {:error, :timeout}
    end
  end

  # The rest of the body, read to its end
  @doc false
  def read_body(conn, timeout), do: read_rest(conn, timeout, [])

  defp read_rest(conn, timeout, acc) do
    case next(conn, timeout) do
      {:data, data} -> read_rest(conn, timeout, [acc | data])
      :done -> {:ok, IO.iodata_to_binary(acc)}
      {:error, _reason} = error -> error
    end
  end

  # Stops the request, unless it's over, and drops its events on their way
  @doc false
  def close(conn) do
    JS.run(@abort_js, %{controller: conn.controller})
    flush_bridge(conn.id)
    :ok
  end

  if Code.ensure_loaded?(Req) do
    @doc "Returns the adapter value for the installed Req version."
    def adapter do
      version = :req |> Application.spec(:vsn) |> to_string()

      if Version.match?(version, ">= 0.7.0") do
        Popcorn.Fetch.Req
      else
        &Popcorn.Fetch.Req.run/1
      end
    end
  end

  defp put_body(args, nil), do: args
  defp put_body(args, body), do: Map.put(args, :body, Base.encode64(body))

  defp decode(%{"event" => "status", "status" => status, "headers" => headers}) do
    headers = Enum.map(headers, fn [name, value] -> {name, value} end)
    {:head, status, decompressed(headers)}
  end

  defp decode(%{"event" => "chunk", "data" => data}), do: {:data, Base.decode64!(data)}
  defp decode(%{"event" => "done"}), do: :done
  defp decode(%{"event" => "error", "error" => message}), do: {:error, {:fetch, hint(message)}}

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

  # The browser decompresses bodies, which these headers describe compressed. Req
  # wouldn't decode a body that they say is compressed.
  defp decompressed(headers) do
    if List.keymember?(headers, "content-encoding", 0) do
      Enum.reject(headers, fn {name, _value} -> name in ["content-encoding", "content-length"] end)
    else
      headers
    end
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

  # The wait for an event: its own timeout, within the request's deadline
  defp wait(:infinity, timeout), do: timeout

  defp wait(deadline, timeout) do
    remaining = max(0, deadline - System.monotonic_time(:millisecond))
    if timeout == :infinity, do: remaining, else: min(remaining, timeout)
  end
end
