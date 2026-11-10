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
    @compile {:no_warn_undefined,
              [Req.Adapter, Req.Fields, Req.Response, Req.Response.Async, Req.TransportError]}

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
    def run(request) do
      case legacy_normalize_body(request) do
        {:ok, body, request} ->
          legacy_run(request, body)

        {:halt, request} ->
          {request, Req.Response.new(status: nil)}
      end
    end

    defp legacy_run(request, body) do
      req = %{
        method: request.method |> to_string() |> String.upcase(),
        url: URI.to_string(request.url),
        headers: request_headers(request),
        body: body
      }

      timeout = Map.get(request.options, :receive_timeout, @default_timeout)

      case request.into do
        :self -> legacy_run_into_self(request, req, timeout)
        into -> legacy_run_into(request, req, into, timeout)
      end
    end

    # Req's timeout bounds each event, rather than the whole response
    defp legacy_run_into(request, req, into, timeout) do
      with {:ok, conn} <- start(req, timeout: :infinity),
           {:head, status, headers} <- next(conn, timeout) do
        response = Req.Response.new(status: status, headers: headers)

        case into do
          nil -> legacy_into_body(request, response, conn, timeout)
          fun when is_function(fun, 2) -> legacy_into_fun(request, response, conn, timeout, fun)
          collectable -> legacy_into_collectable(request, response, conn, timeout, collectable)
        end
      else
        {:error, reason} -> legacy_transport_error(request, reason)
      end
    end

    defp legacy_into_body(request, response, conn, timeout) do
      case read_body(conn, timeout) do
        {:ok, body} -> {request, %{response | body: body}}
        {:error, reason} -> legacy_transport_error(request, reason)
      end
    end

    defp legacy_into_fun(request, response, conn, timeout, fun) do
      case legacy_read_into(conn, timeout, fun, {request, response}) do
        {:ok, acc} -> acc
        {:error, reason, {request, _response}} -> legacy_transport_error(request, reason)
      end
    end

    defp legacy_into_collectable(request, response, conn, timeout, collectable) do
      collectable = if response.status == 200, do: collectable, else: ""
      {acc, collector} = Collectable.into(collectable)

      fun = fn {:data, data}, {request, {acc, response}} ->
        {:cont, {request, {collector.(acc, {:cont, data}), response}}}
      end

      case legacy_read_into(conn, timeout, fun, {request, {acc, response}}) do
        {:ok, {request, {acc, response}}} ->
          {request, %{response | body: collector.(acc, :done)}}

        {:error, reason, {request, {acc, _response}}} ->
          collector.(acc, :halt)
          legacy_transport_error(request, reason)
      end
    end

    # Hands the body's chunks to `fun`, until it halts or the body ends
    defp legacy_read_into(conn, timeout, fun, acc) do
      case next(conn, timeout) do
        {:data, _data} = event ->
          case fun.(event, acc) do
            {:cont, acc} ->
              legacy_read_into(conn, timeout, fun, acc)

            {:halt, acc} ->
              close(conn)
              {:ok, acc}

            other ->
              close(conn)
              raise ArgumentError, "expected {:cont, acc} or {:halt, acc}, got: #{inspect(other)}"
          end

        :done ->
          {:ok, acc}

        {:error, reason} ->
          {:error, reason, acc}
      end
    end

    defp legacy_run_into_self(request, req, timeout) do
      ref = make_ref()

      case start_relay(req, ref, timeout) do
        {:ok, status, headers, relay} ->
          async = async_body(self(), ref, relay)
          {request, Req.Response.new(status: status, headers: headers, body: async)}

        {:error, reason} ->
          legacy_transport_error(request, reason)
      end
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
      req = %{
        method: request.method |> to_string() |> String.upcase(),
        url: URI.to_string(request.url),
        headers: Req.Fields.get_list(request.headers),
        body: body
      }

      timeout = Map.get(request.options, :receive_timeout, @default_timeout)

      case request.into do
        :self -> stream_into_self(response, req, acc, fun, state, timeout)
        _other -> stream_response(response, req, acc, fun, state, timeout)
      end
    end

    # Req's timeout bounds each event, rather than the whole response
    defp stream_response(response, req, acc, fun, state, timeout) do
      with {:ok, conn} <- start(req, timeout: :infinity),
           {:head, status, headers} <- next(conn, timeout) do
        response = %{response | status: status}

        case fun.({:status, status}, response, acc, state) do
          {:ok, response, acc, state} ->
            headers_field = Req.Fields.new_without_normalize_with_duplicates(headers)
            response = %{response | headers: headers_field}

            case fun.({:headers, headers}, response, acc, state) do
              {:ok, response, acc, state} ->
                stream_body(response, acc, fun, state, conn, timeout)

              {:halt, _, _, _} = result ->
                close(conn)
                result

              {{:error, _exception}, _, _, _} = result ->
                close(conn)
                result
            end

          {:halt, _, _, _} = result ->
            close(conn)
            result

          {{:error, _exception}, _, _, _} = result ->
            close(conn)
            result
        end
      else
        {:error, reason} -> transport_error(response, acc, state, reason)
      end
    end

    defp stream_body(response, acc, fun, state, conn, timeout) do
      case next(conn, timeout) do
        {:data, data} ->
          case fun.({:data, data}, response, acc, state) do
            {:ok, response, acc, state} ->
              stream_body(response, acc, fun, state, conn, timeout)

            {:halt, _, _, _} = result ->
              close(conn)
              result

            {{:error, _exception}, _, _, _} = result ->
              close(conn)
              result
          end

        :done ->
          {:ok, response, acc, state}

        {:error, reason} ->
          transport_error(response, acc, state, reason)
      end
    end

    defp stream_into_self(response, req, acc, fun, state, timeout) do
      ref = make_ref()

      case start_relay(req, ref, timeout) do
        {:ok, status, headers, relay} ->
          response = %{response | status: status}

          case fun.({:status, status}, response, acc, state) do
            {:ok, response, acc, state} ->
              headers_field = Req.Fields.new_without_normalize_with_duplicates(headers)
              response = %{response | headers: headers_field}

              case fun.({:headers, headers}, response, acc, state) do
                {:ok, response, acc, state} ->
                  {:ok, %{response | body: async_body(self(), ref, relay)}, acc, state}

                {:halt, _, _, _} = result ->
                  stop_relay(relay, ref)
                  result

                {{:error, _exception}, _, _, _} = result ->
                  stop_relay(relay, ref)
                  result
              end

            {:halt, _, _, _} = result ->
              stop_relay(relay, ref)
              result

            {{:error, _exception}, _, _, _} = result ->
              stop_relay(relay, ref)
              result
          end

        {:error, reason} ->
          transport_error(response, acc, state, reason)
      end
    end

    # Req's `:self` responses arrive as messages to the caller, which a relay
    # process sends as it reads the request's events
    defp start_relay(req, ref, timeout) do
      owner = self()

      {pid, monitor} =
        spawn_monitor(fn ->
          receive do
            {^ref, conn} -> relay(owner, ref, conn, timeout)
          end
        end)

      result =
        case start(req, target: pid, timeout: :infinity) do
          {:ok, conn} ->
            send(pid, {ref, conn})
            relay = %{pid: pid, conn: conn}

            with {:ok, status, headers} <- await_head(ref, relay, monitor) do
              {:ok, status, headers, relay}
            end

          {:error, _reason} = error ->
            Process.exit(pid, :kill)
            error
        end

      Process.demonitor(monitor, [:flush])
      result
    end

    defp await_head(ref, relay, monitor) do
      receive do
        {^ref, {:head, status, headers}} ->
          {:ok, status, headers}

        {^ref, {:error, reason}} ->
          {:error, reason}

        # The relay bounds its own wait, so this only fires if it crashed.
        {:DOWN, ^monitor, :process, _pid, reason} ->
          close(relay.conn)
          {:error, {:relay_down, reason}}
      end
    end

    # Req's timeout bounds each event, and the request ends with `:done` or an error
    defp relay(owner, ref, conn, timeout) do
      event = next(conn, timeout)
      send(owner, {ref, event})

      case event do
        {:head, _status, _headers} -> relay(owner, ref, conn, timeout)
        {:data, _data} -> relay(owner, ref, conn, timeout)
        _end -> :ok
      end
    end

    defp stop_relay(relay, ref) do
      Process.exit(relay.pid, :kill)
      close(relay.conn)
      flush(ref)
    end

    defp async_body(owner, ref, relay) do
      cancel_fun = fn ref ->
        stop_relay(relay, ref)
        :ok
      end

      struct!(Req.Response.Async,
        pid: owner,
        ref: ref,
        stream_fun: &parse_message/2,
        cancel_fun: cancel_fun
      )
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

    defp flush(ref) do
      receive do
        {^ref, _} -> flush(ref)
      after
        0 -> :ok
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
