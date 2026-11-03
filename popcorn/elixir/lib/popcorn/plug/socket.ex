defmodule Popcorn.Plug.Socket do
  @moduledoc false
  # Drives a WebSock handler whose other end is JavaScript, as a web server's
  # WebSocket connection process does for a client. See Popcorn.Plug.

  # The idle timeout when the upgrade doesn't set one, as in Bandit
  @default_timeout 60_000

  def run(websock, state, opts, id) do
    if fullsweep_after = opts[:fullsweep_after] do
      Process.flag(:fullsweep_after, fullsweep_after)
    end

    if max_heap_size = opts[:max_heap_size] do
      Process.flag(:max_heap_size, max_heap_size)
    end

    socket = %{websock: websock, id: id, timeout: Keyword.get(opts, :timeout, @default_timeout)}
    handle(socket, :init, [state])
  end

  defp loop(socket, state) do
    receive do
      {:wasm, %{"text" => data}} ->
        handle(socket, :handle_in, [{data, opcode: :text}, state])

      {:wasm, %{"binary" => data}} ->
        handle(socket, :handle_in, [{data, opcode: :binary}, state])

      {:wasm, %{"close" => _code}} ->
        terminate(socket, :remote, state)

      message ->
        handle(socket, :handle_info, [message, state])
    after
      socket.timeout ->
        close(socket, 1000)
        terminate(socket, :timeout, state)
    end
  end

  defp handle(socket, callback, args) do
    result =
      try do
        apply(socket.websock, callback, args)
      catch
        kind, reason ->
          close(socket, 1011)
          :erlang.raise(kind, reason, __STACKTRACE__)
      end

    case result do
      {:ok, state} ->
        loop(socket, state)

      {:push, messages, state} ->
        push(socket, messages)
        loop(socket, state)

      {:reply, _status, messages, state} ->
        push(socket, messages)
        loop(socket, state)

      {:stop, reason, state} ->
        stop(socket, reason, 1000, [], state)

      {:stop, reason, detail, state} ->
        stop(socket, reason, detail, [], state)

      {:stop, reason, detail, messages, state} ->
        stop(socket, reason, detail, messages, state)
    end
  end

  defp stop(socket, reason, detail, messages, state) do
    push(socket, messages)
    close(socket, if(is_integer(detail), do: detail, else: elem(detail, 0)))

    reason =
      case reason do
        :normal -> :normal
        {:shutdown, :restart} -> :shutdown
        reason -> {:error, reason}
      end

    terminate(socket, reason, state)
  end

  defp terminate(socket, reason, state) do
    if function_exported?(socket.websock, :terminate, 2) do
      socket.websock.terminate(reason, state)
    end

    :ok
  end

  defp push(socket, messages) do
    for {opcode, data} <- List.wrap(messages), opcode in [:text, :binary] do
      Popcorn.JS.send(%{:popcorn_socket => socket.id, opcode => IO.iodata_to_binary(data || "")})
    end
  end

  defp close(socket, code) do
    Popcorn.JS.send(%{popcorn_socket: socket.id, close: code})
  end
end
