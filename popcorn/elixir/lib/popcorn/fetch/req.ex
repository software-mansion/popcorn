if Code.ensure_loaded?(Req) do
  defmodule Popcorn.Fetch.Req do
    @moduledoc false

    # Req's adapter, on top of Popcorn.Fetch's internal API. Popcorn.Fetch.adapter/0
    # returns it in the form the installed Req version expects.

    alias Popcorn.Fetch

    @default_receive_timeout 30_000

    @compile {:no_warn_undefined,
              [Req.Adapter, Req.Fields, Req.Response, Req.Response.Async, Req.TransportError]}

    if Code.ensure_loaded?(Req.Adapter) do
      @behaviour Req.Adapter
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

      timeout = Map.get(request.options, :receive_timeout, @default_receive_timeout)

      case request.into do
        :self -> legacy_run_into_self(request, req, timeout)
        into -> legacy_run_into(request, req, into, timeout)
      end
    end

    # Req's timeout bounds each event, rather than the whole response
    defp legacy_run_into(request, req, into, timeout) do
      with {:ok, conn} <- Fetch.start(req, timeout: :infinity),
           {:head, status, headers} <- Fetch.next(conn, timeout) do
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
      case Fetch.read_body(conn, timeout) do
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
      case Fetch.next(conn, timeout) do
        {:data, _data} = event ->
          case fun.(event, acc) do
            {:cont, acc} ->
              legacy_read_into(conn, timeout, fun, acc)

            {:halt, acc} ->
              Fetch.close(conn)
              {:ok, acc}

            other ->
              Fetch.close(conn)
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

      timeout = Map.get(request.options, :receive_timeout, @default_receive_timeout)

      case request.into do
        :self -> stream_into_self(response, req, acc, fun, state, timeout)
        _other -> stream_response(response, req, acc, fun, state, timeout)
      end
    end

    # Req's timeout bounds each event, rather than the whole response
    defp stream_response(response, req, acc, fun, state, timeout) do
      with {:ok, conn} <- Fetch.start(req, timeout: :infinity),
           {:head, status, headers} <- Fetch.next(conn, timeout) do
        response = %{response | status: status}

        case fun.({:status, status}, response, acc, state) do
          {:ok, response, acc, state} ->
            headers_field = Req.Fields.new_without_normalize_with_duplicates(headers)
            response = %{response | headers: headers_field}

            case fun.({:headers, headers}, response, acc, state) do
              {:ok, response, acc, state} ->
                stream_body(response, acc, fun, state, conn, timeout)

              {:halt, _, _, _} = result ->
                Fetch.close(conn)
                result

              {{:error, _exception}, _, _, _} = result ->
                Fetch.close(conn)
                result
            end

          {:halt, _, _, _} = result ->
            Fetch.close(conn)
            result

          {{:error, _exception}, _, _, _} = result ->
            Fetch.close(conn)
            result
        end
      else
        {:error, reason} -> transport_error(response, acc, state, reason)
      end
    end

    defp stream_body(response, acc, fun, state, conn, timeout) do
      case Fetch.next(conn, timeout) do
        {:data, data} ->
          case fun.({:data, data}, response, acc, state) do
            {:ok, response, acc, state} ->
              stream_body(response, acc, fun, state, conn, timeout)

            {:halt, _, _, _} = result ->
              Fetch.close(conn)
              result

            {{:error, _exception}, _, _, _} = result ->
              Fetch.close(conn)
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
        case Fetch.start(req, target: pid, timeout: :infinity) do
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
          Fetch.close(relay.conn)
          {:error, {:relay_down, reason}}
      end
    end

    # Req's timeout bounds each event, and the request ends with `:done` or an error
    defp relay(owner, ref, conn, timeout) do
      event = Fetch.next(conn, timeout)
      send(owner, {ref, event})

      case event do
        {:head, _status, _headers} -> relay(owner, ref, conn, timeout)
        {:data, _data} -> relay(owner, ref, conn, timeout)
        _end -> :ok
      end
    end

    defp stop_relay(relay, ref) do
      Process.exit(relay.pid, :kill)
      Fetch.close(relay.conn)
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
end
