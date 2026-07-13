defmodule Popcorn.Fetch do
  @moduledoc """
  A Req adapter that runs HTTP requests through the browser's `fetch()`.

  Req's default `Req.Finch` adapter uses TCP sockets, which are not available in WebAssembly.
  Popcorn installs this module as the default adapter instead.

  Use
  ```elixir
  # config/config.exs
  config :req, default_options: [adapter: CustomAdapter]
  ```

  to set a custom adapter.

  ## Using it without Req

  `request/2` is the fetch primitive the adapter sits on:

  ```elixir
  args = %{method: "GET", url: "https://api.example.com/thing"}
  {:ok, %{status: 200, body: body}} = Popcorn.Fetch.request(args)
  ```

  ## Notes

  `fetch()` has some limitations:
  - Popcorn requires CORS headers to work. This may affect requests sent to external domains.
  - Browsers automatically follow redirects. `:max_redirects` and `:redirect_log_level` request options have no effect.
  - Browsers deal with compression. `decompress_body` step can't be hooked into. `raw` request option has no effect.
  - Some headers are dropped. This includes `Host`, `Referer`, `Cookie`, and others.
  """

  # Req is an optional dependency
  @compile {:no_warn_undefined,
            [Req.Fields, Req.Response, Req.Response.Async, Req.TransportError]}

  alias Popcorn.Wasm

  @default_timeout 30_000

  @type request :: %{
          required(:method) => String.t(),
          required(:url) => String.t(),
          optional(:headers) => [{String.t(), String.t()}],
          optional(:body) => binary() | nil
        }

  @type response :: %{
          status: non_neg_integer(),
          headers: [{String.t(), String.t()}],
          body: binary()
        }

  @typedoc """
  - `:timeout` - no reply within `:timeout`. The browser request is aborted.
  - `{:fetch, message}` - the browser rejected the request.
  - `{:bridge, reason}` - the call into JavaScript itself failed. See `Popcorn.Wasm.run_js/3`.
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
  Performs an HTTP request and returns the whole response.

  ## Options

  - `:timeout` - how long to wait for the response to complete, in milliseconds.
                 Defaults to `#{@default_timeout}`. Request is aborted on timeout.

  ## Examples

  ```elixir
  Fetch.request(%{method: "GET", url: "/api/status"})
  #=> {:ok, %{status: 200, headers: [{"content-type", "application/json"}], body: "{}"}}

  Fetch.request(%{method: "POST", url: "/api/blobs", body: <<255, 0, 65>>})
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

  @doc false
  def run(request) do
    case normalize_body(request) do
      {:ok, body, request} ->
        run(request, body)

      # A halted req_body_fun means "close the connection without reading a
      # response", which has no fetch equivalent beyond never starting.
      {:halt, request} ->
        {request, Req.Response.new(status: nil)}
    end
  end

  defp run(request, body) do
    method = request.method |> to_string() |> String.upcase()

    headers =
      request.headers
      |> Req.Fields.get_list()
      |> Enum.map(fn {name, value} -> [name, value] end)

    req = %{
      method: method,
      url: URI.to_string(request.url),
      headers: headers,
      body: body
    }

    timeout = Map.get(request.options, :receive_timeout, @default_timeout)

    case request.into do
      :self -> run_into_self(request, req, timeout)
      into -> run_into(request, req, into, timeout)
    end
  end

  defp run_into(request, req, into, timeout) do
    wait = {:each, timeout}

    with {:ok, handle} <- start(req, self()),
         {:ok, status, headers} <- collect_head(handle, wait) do
      response = Req.Response.new(status: status, headers: headers)

      case into do
        nil -> into_body(request, response, handle, wait)
        fun when is_function(fun, 2) -> into_fun(request, response, handle, wait, fun)
        collectable -> into_collectable(request, response, handle, wait, collectable)
      end
    else
      {:error, reason} -> transport_error(request, reason)
    end
  end

  defp into_body(request, response, handle, wait) do
    case collect_body(handle, wait) do
      {:ok, body} -> {request, %{response | body: body}}
      {:error, reason} -> transport_error(request, reason)
    end
  end

  defp into_fun(request, response, handle, wait, fun) do
    result =
      handle
      |> body_stream(wait)
      |> Enum.reduce_while({:ok, {request, response}}, wrapped_reducer(fun))

    case result do
      {:ok, acc} ->
        acc

      {:error, reason, {request, _response}} ->
        transport_error(request, reason)
    end
  end

  defp into_collectable(request, response, handle, wait, collectable) do
    collectable = if response.status == 200, do: collectable, else: ""
    {acc, collector} = Collectable.into(collectable)

    fun = fn {:data, data}, {request, {acc, response}} ->
      acc = collector.(acc, {:cont, data})
      {:cont, {request, {acc, response}}}
    end

    result =
      handle
      |> body_stream(wait)
      |> Enum.reduce_while({:ok, {request, {acc, response}}}, wrapped_reducer(fun))

    case result do
      {:ok, {request, {acc, response}}} ->
        {request, %{response | body: collector.(acc, :done)}}

      {:error, reason, {request, {acc, _response}}} ->
        collector.(acc, :halt)
        transport_error(request, reason)
    end
  end

  defp wrapped_reducer(fun) do
    fn
      {:data, _data} = event, {:ok, acc} ->
        case fun.(event, acc) do
          {:cont, acc} ->
            {:cont, {:ok, acc}}

          {:halt, acc} ->
            {:halt, {:ok, acc}}

          other ->
            raise ArgumentError,
                  "expected {:cont, acc} or {:halt, acc}, got: #{inspect(other)}"
        end

      {:error, reason}, {:ok, acc} ->
        {:halt, {:error, reason, acc}}
    end
  end

  defp body_stream(handle, wait) do
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

  defp run_into_self(request, req, timeout) do
    ref = make_ref()
    owner = self()
    {relay, monitor} = spawn_monitor(fn -> relay(owner, ref, timeout) end)

    result =
      with {:ok, handle} <- start(req, relay),
           {:ok, status, headers} <- await_head(ref, relay, monitor, handle) do
        async = async_body(owner, ref, relay, handle)
        {request, Req.Response.new(status: status, headers: headers, body: async)}
      else
        {:error, reason} ->
          send(relay, :cancel)
          transport_error(request, reason)
      end

    Process.demonitor(monitor, [:flush])
    result
  end

  defp await_head(ref, relay, monitor, handle) do
    receive do
      {^ref, {:head, status, headers}} ->
        {:ok, status, headers}

      {^ref, {:error, reason}} ->
        cancel(handle)
        {:error, reason}

      # The relay bounds its own wait, so this only fires if it crashed.
      {:DOWN, ^monitor, :process, ^relay, reason} ->
        cancel(handle)
        {:error, {:relay_down, reason}}
    end
  end

  defp async_body(owner, ref, relay, handle) do
    stream_fun = fn ref, message ->
      case parse_message(ref, message) do
        {:error, _} = error ->
          cancel(handle)
          error

        result ->
          result
      end
    end

    cancel_fun = fn ref ->
      send(relay, :cancel)
      cancel(handle)
      flush(ref)
      :ok
    end

    struct!(Req.Response.Async,
      pid: owner,
      ref: ref,
      stream_fun: stream_fun,
      cancel_fun: cancel_fun
    )
  end

  # Used for bridge -> `Req.Response.Async` translation.
  # The timeout is per message.
  defp relay(owner, ref, timeout) do
    receive do
      {:wasm, %{"popcorn_fetch" => _} = message} ->
        case decode(message) do
          {:head, status, headers} ->
            send(owner, {ref, {:head, status, headers}})
            relay(owner, ref, timeout)

          {:chunk, data} ->
            send(owner, {ref, {:data, data}})
            relay(owner, ref, timeout)

          :done ->
            send(owner, {ref, :done})

          {:error, reason} ->
            send(owner, {ref, {:error, reason}})
        end

      :cancel ->
        :ok
    after
      timeout -> send(owner, {ref, {:error, :timeout}})
    end
  end

  defp parse_message(ref, {ref, {:data, data}}), do: {:ok, [data: data]}
  defp parse_message(ref, {ref, :done}), do: {:ok, [:done]}

  defp parse_message(ref, {ref, {:error, reason}}) do
    {:error, Req.TransportError.exception(reason: reason)}
  end

  defp parse_message(_ref, _message), do: :unknown

  defp transport_error(request, reason) do
    {request, Req.TransportError.exception(reason: reason)}
  end

  defp normalize_body(request) do
    case request.body do
      nil ->
        {:ok, nil, request}

      iodata when is_binary(iodata) or is_list(iodata) ->
        {:ok, IO.iodata_to_binary(iodata), request}

      req_body_fun when is_function(req_body_fun, 1) ->
        drain_body(req_body_fun, request)

      enumerable ->
        {:ok, enumerable |> Enum.to_list() |> IO.iodata_to_binary(), request}
    end
  end

  defp drain_body(req_body_fun, request, chunks \\ []) do
    case req_body_fun.(request) do
      {:data, chunk, request} ->
        drain_body(req_body_fun, request, [chunk | chunks])

      {:done, request} ->
        binary = chunks |> Enum.reverse() |> IO.iodata_to_binary()
        {:ok, binary, request}

      {:halt, request} ->
        {:halt, request}

      other ->
        raise ArgumentError, """
        expected req_body_fun to return {:data, chunk, request}, {:done, request},
        or {:halt, request}, got: #{inspect(other)}
        """
    end
  end

  defp start(req, target) do
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

    case Wasm.run_js(@start_js, args) do
      {:ok, controller} -> {:ok, %{id: id, target: target, controller: controller}}
      {:error, reason} -> {:error, {:bridge, reason}}
    end
  end

  defp put_body(args, nil), do: args
  defp put_body(args, body), do: Map.put(args, :body, Base.encode64(body))

  defp collect_head(handle, wait) do
    case recv(handle, wait) do
      {:head, status, headers} ->
        {:ok, status, headers}

      {:error, reason} ->
        cancel(handle)
        {:error, reason}
    end
  end

  defp collect_body(handle, wait) do
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

  defp decode(%{"event" => "status", "status" => status, "headers" => headers}) do
    {:head, status, Enum.map(headers, fn [name, value] -> {name, value} end)}
  end

  defp decode(%{"event" => "chunk", "data" => data}), do: {:chunk, Base.decode64!(data)}
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

  defp cancel(handle) do
    Wasm.run_js(@abort_js, %{controller: handle.controller})
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

  defp flush(ref) do
    receive do
      {^ref, _} -> flush(ref)
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
