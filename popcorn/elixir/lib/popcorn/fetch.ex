defmodule Popcorn.Fetch do
  @moduledoc """
  Sends HTTP requests through the browser's `fetch()` API.

  ## Using Req

  Req is optional. If your application includes Req, Popcorn installs this adapter when it starts inside the Popcorn runtime.
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
  - The browser decompresses responses. Req's `:raw` option cannot preserve compressed response bytes.
  - The browser controls restricted headers, such as `Host` and `Cookie`.
  """

  # Req is an optional dependency
  @compile {:no_warn_undefined,
            [Req.Adapter, Req.Fields, Req.Response, Req.Response.Async, Req.TransportError]}

  if Code.ensure_loaded?(Req.Adapter) do
    @behaviour Req.Adapter
  end

  alias Popcorn.Wasm

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
  - `{:bridge, reason}` - JavaScript execution failed. See `Popcorn.Wasm.run_js/3`.
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
  def run(request) do
    case legacy_normalize_body(request) do
      {:ok, body, request} ->
        legacy_run(request, body)

      {:halt, request} ->
        {request, Req.Response.new(status: nil)}
    end
  end

  defp legacy_run(request, body) do
    method = request.method |> to_string() |> String.upcase()

    headers = Enum.map(request_headers(request), fn {name, value} -> [name, value] end)

    req = %{
      method: method,
      url: URI.to_string(request.url),
      headers: headers,
      body: body
    }

    timeout = Map.get(request.options, :receive_timeout, @default_timeout)

    case request.into do
      :self -> legacy_run_into_self(request, req, timeout)
      into -> legacy_run_into(request, req, into, timeout)
    end
  end

  defp legacy_run_into(request, req, into, timeout) do
    wait = {:each, timeout}

    with {:ok, handle} <- start(req, self()),
         {:ok, status, headers} <- collect_head(handle, wait) do
      response = Req.Response.new(status: status, headers: headers)

      case into do
        nil -> legacy_into_body(request, response, handle, wait)
        fun when is_function(fun, 2) -> legacy_into_fun(request, response, handle, wait, fun)
        collectable -> legacy_into_collectable(request, response, handle, wait, collectable)
      end
    else
      {:error, reason} -> legacy_transport_error(request, reason)
    end
  end

  defp legacy_into_body(request, response, handle, wait) do
    case collect_body(handle, wait) do
      {:ok, body} -> {request, %{response | body: body}}
      {:error, reason} -> legacy_transport_error(request, reason)
    end
  end

  defp legacy_into_fun(request, response, handle, wait, fun) do
    result =
      handle
      |> body_stream(wait)
      |> Enum.reduce_while({:ok, {request, response}}, legacy_wrapped_reducer(fun))

    case result do
      {:ok, acc} -> acc
      {:error, reason, {request, _response}} -> legacy_transport_error(request, reason)
    end
  end

  defp legacy_into_collectable(request, response, handle, wait, collectable) do
    collectable = if response.status == 200, do: collectable, else: ""
    {acc, collector} = Collectable.into(collectable)

    fun = fn {:data, data}, {request, {acc, response}} ->
      acc = collector.(acc, {:cont, data})
      {:cont, {request, {acc, response}}}
    end

    result =
      handle
      |> body_stream(wait)
      |> Enum.reduce_while({:ok, {request, {acc, response}}}, legacy_wrapped_reducer(fun))

    case result do
      {:ok, {request, {acc, response}}} ->
        {request, %{response | body: collector.(acc, :done)}}

      {:error, reason, {request, {acc, _response}}} ->
        collector.(acc, :halt)
        legacy_transport_error(request, reason)
    end
  end

  defp legacy_wrapped_reducer(fun) do
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

  defp legacy_run_into_self(request, req, timeout) do
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
          legacy_transport_error(request, reason)
      end

    Process.demonitor(monitor, [:flush])
    result
  end

  if Code.ensure_loaded?(Req.Adapter) do
    @impl Req.Adapter
  end

  @doc false
  def stream(request, acc, fun, state) when is_function(fun, 4) do
    response = Req.Response.new(status: nil, body: nil, request: request)

    case normalize_body(request.body, acc) do
      {:ok, body, acc} ->
        stream(request, response, body, acc, fun, state)

      {:halt, acc} ->
        {:halt, response, acc, state}

      {{:error, exception}, acc} ->
        {{:error, exception}, response, acc, state}
    end
  end

  defp stream(request, response, body, acc, fun, state) do
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
      :self -> stream_into_self(response, req, acc, fun, state, timeout)
      _other -> stream_response(response, req, acc, fun, state, timeout)
    end
  end

  defp stream_response(response, req, acc, fun, state, timeout) do
    wait = {:each, timeout}

    with {:ok, handle} <- start(req, self()),
         {:ok, status, headers} <- collect_head(handle, wait) do
      response = %{response | status: status}

      case fun.({:status, status}, response, acc, state) do
        {:ok, response, acc, state} ->
          headers_field = Req.Fields.new_without_normalize_with_duplicates(headers)
          response = %{response | headers: headers_field}

          case fun.({:headers, headers}, response, acc, state) do
            {:ok, response, acc, state} ->
              stream_body(response, acc, fun, state, handle, wait)

            {:halt, _, _, _} = result ->
              cancel(handle)
              result

            {{:error, _exception}, _, _, _} = result ->
              cancel(handle)
              result
          end

        {:halt, _, _, _} = result ->
          cancel(handle)
          result

        {{:error, _exception}, _, _, _} = result ->
          cancel(handle)
          result
      end
    else
      {:error, reason} -> transport_error(response, acc, state, reason)
    end
  end

  defp stream_body(response, acc, fun, state, handle, wait) do
    handle
    |> body_stream(wait)
    |> Enum.reduce_while({:ok, response, acc, state}, fn
      {:data, data}, {:ok, response, acc, state} ->
        case fun.({:data, data}, response, acc, state) do
          {:ok, response, acc, state} ->
            {:cont, {:ok, response, acc, state}}

          {:halt, response, acc, state} ->
            {:halt, {:halt, response, acc, state}}

          {{:error, _exception}, _, _, _} = result ->
            {:halt, result}
        end

      {:error, reason}, {:ok, response, acc, state} ->
        {:halt, transport_error(response, acc, state, reason)}
    end)
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

  defp stream_into_self(response, req, acc, fun, state, timeout) do
    ref = make_ref()
    owner = self()
    {relay, monitor} = spawn_monitor(fn -> relay(owner, ref, timeout) end)

    result =
      with {:ok, handle} <- start(req, relay),
           {:ok, status, headers} <- await_head(ref, relay, monitor, handle) do
        response = %{response | status: status}

        case fun.({:status, status}, response, acc, state) do
          {:ok, response, acc, state} ->
            headers_field = Req.Fields.new_without_normalize_with_duplicates(headers)
            response = %{response | headers: headers_field}

            case fun.({:headers, headers}, response, acc, state) do
              {:ok, response, acc, state} ->
                async = async_body(owner, ref, relay, handle)
                {:ok, %{response | body: async}, acc, state}

              {:halt, _, _, _} = result ->
                cancel_async(relay, handle)
                result

              {{:error, _exception}, _, _, _} = result ->
                cancel_async(relay, handle)
                result
            end

          {:halt, _, _, _} = result ->
            cancel_async(relay, handle)
            result

          {{:error, _exception}, _, _, _} = result ->
            cancel_async(relay, handle)
            result
        end
      else
        {:error, reason} ->
          send(relay, :cancel)
          transport_error(response, acc, state, reason)
      end

    Process.demonitor(monitor, [:flush])
    result
  end

  defp cancel_async(relay, handle) do
    send(relay, :cancel)
    cancel(handle)
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

  defp transport_error(response, acc, state, reason) do
    exception = Req.TransportError.exception(reason: reason)
    {{:error, exception}, response, acc, state}
  end

  defp legacy_transport_error(request, reason) do
    {request, Req.TransportError.exception(reason: reason)}
  end

  defp request_headers(request) do
    if Code.ensure_loaded?(Req.Fields) do
      Req.Fields.get_list(request.headers)
    else
      for {name, values} <- request.headers,
          value <- List.wrap(values) do
        {name, value}
      end
    end
  end

  defp legacy_normalize_body(request) do
    case request.body do
      nil ->
        {:ok, nil, request}

      iodata when is_binary(iodata) or is_list(iodata) ->
        {:ok, IO.iodata_to_binary(iodata), request}

      req_body_fun when is_function(req_body_fun, 1) ->
        legacy_drain_body(req_body_fun, request)

      enumerable ->
        {:ok, enumerable |> Enum.to_list() |> IO.iodata_to_binary(), request}
    end
  end

  defp legacy_drain_body(req_body_fun, request, chunks \\ []) do
    case req_body_fun.(request) do
      {:data, chunk, request} ->
        legacy_drain_body(req_body_fun, request, [chunk | chunks])

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

  defp normalize_body(nil, acc), do: {:ok, nil, acc}

  defp normalize_body(iodata, acc) when is_binary(iodata) or is_list(iodata) do
    {:ok, IO.iodata_to_binary(iodata), acc}
  end

  defp normalize_body(req_body_fun, acc) when is_function(req_body_fun, 1) do
    drain_body(req_body_fun, acc)
  end

  defp normalize_body(enumerable, acc) do
    {:ok, enumerable |> Enum.to_list() |> IO.iodata_to_binary(), acc}
  end

  defp drain_body(req_body_fun, acc, chunks \\ []) do
    case req_body_fun.(acc) do
      {:data, chunk, acc} ->
        drain_body(req_body_fun, acc, [chunk | chunks])

      {:done, chunk, acc} ->
        binary = [chunk | chunks] |> Enum.reverse() |> IO.iodata_to_binary()
        {:ok, binary, acc}

      {:done, acc} ->
        binary = chunks |> Enum.reverse() |> IO.iodata_to_binary()
        {:ok, binary, acc}

      {:halt, acc} ->
        {:halt, acc}

      {:error, exception, acc} ->
        {{:error, exception}, acc}

      other ->
        raise "expected req_body_fun to return {:data, chunk, acc}, {:done, chunk, acc}, " <>
                "{:done, acc}, {:halt, acc}, or {:error, exception, acc}, got: #{inspect(other)}"
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
