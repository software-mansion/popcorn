defmodule Popcorn.Fetch.ReqAdapter do
  @moduledoc false
  # Req's adapter, which `Popcorn.Fetch.run/1` and `Popcorn.Fetch.stream/4` call.
  # It calls Req only at run time, as Req may come after Popcorn: Mix.install/2
  # can install it. The packager leaves the module out of packages that have
  # neither Req nor Mix.

  @compile {:no_warn_undefined,
            [Req.Fields, Req.Response, Req.Response.Async, Req.TransportError]}

  import Popcorn.Fetch,
    only: [
      body_stream: 2,
      cancel: 1,
      collect_body: 2,
      collect_head: 2,
      decode: 1,
      default_timeout: 0,
      start: 2
    ]

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

    timeout = Map.get(request.options, :receive_timeout, default_timeout())

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

    timeout = Map.get(request.options, :receive_timeout, default_timeout())

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

  defp flush(ref) do
    receive do
      {^ref, _} -> flush(ref)
    after
      0 -> :ok
    end
  end
end
