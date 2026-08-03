defmodule Popcorn.Proxy do
  @default_name :popcorn_proxy
  @default_timeout_ms 5_000

  @moduledoc """
  Dispatches calls and casts to GenServers running in the VM.

  JS can't build a `GenServer.call/3` itself and needs a proxy to send requests.
  Proxies are multiplexing requests to the GenServers, tracking responses and timeouts.

  By default, it is registered as `#{inspect(@default_name)}`. You can use multiple proxies in your application.

  ## Example
  <!-- TODO: check if correct -->
  ```elixir
  # app/supervisor.ex
  children = [
    MyApp.Counter,
    Popcorn.Proxy
  ]
  ```

  ```elixir
  # gen_server.ex
  defmodule MyApp.Counter do
    def handle_call(["add", n], _from, state) do
      {:reply, state + n, state + n}
    end

    def handle_cast("reset", state) do
      {:noreply, 0}
    end
  end
  ```

  Then, from JavaScript:
  ```js
  // `call()` settles when the response from the GenServer is received.
  const [status, value] = await popcorn.genserver.call("counter", ["add", n: 1]);
  // `cast()` settles when the message is sent to the proxy.
  await popcorn.genserver.cast("counter", "reset");
  ```
  """

  use GenServer

  alias Popcorn.Wasm

  @doc """
  Starts the proxy.

  ## Options

  - `:name` - the registered name JS addresses in `proxy` option. Defaults to `#{inspect(@default_name)}`.
  """
  @spec start_link(keyword()) :: GenServer.on_start()
  def start_link(opts \\ []) do
    {name, opts} = Keyword.pop(opts, :name, @default_name)
    GenServer.start_link(__MODULE__, opts, name: name)
  end

  @impl GenServer
  def init(_opts) do
    {:ok, %{requests: :gen_server.reqids_new(), pending: %{}}}
  end

  @impl GenServer
  def handle_info({:wasm, %{"kind" => "call", "id" => id} = message}, state) do
    request = Map.fetch!(message, "request")
    timeout = Map.get(message, "timeout_ms", @default_timeout_ms)

    case whereis(message["target"]) do
      nil ->
        reply(id, %{ok: false, error: %{kind: "noproc"}})
        {:noreply, state}

      pid ->
        request_id = :gen_server.send_request(pid, request)
        timer = Process.send_after(self(), {:call_timeout, id}, timeout)
        requests = :gen_server.reqids_add(request_id, id, state.requests)
        pending = Map.put(state.pending, id, {request_id, timer})
        {:noreply, %{state | requests: requests, pending: pending}}
    end
  end

  def handle_info({:wasm, %{"kind" => "cast"} = message}, state) do
    case whereis(message["target"]) do
      nil -> :ok
      pid -> GenServer.cast(pid, Map.fetch!(message, "request"))
    end

    {:noreply, state}
  end

  def handle_info({:call_timeout, id}, state) do
    case Map.pop(state.pending, id) do
      {nil, _pending} ->
        {:noreply, state}

      {{request_id, _timer}, pending} ->
        timeout = 0

        result =
          case :gen_server.receive_response(request_id, timeout) do
            {:reply, value} -> {:ok, value}
            {:error, {reason, _server}} -> {:exit, reason}
            :timeout -> :timeout
          end

        state = %{
          state
          | requests: delete_request(state.requests, request_id),
            pending: pending
        }

        send_result(id, result)
        {:noreply, state}
    end
  end

  def handle_info(message, state) do
    case :gen_server.check_response(message, state.requests, true) do
      {{:reply, value}, id, requests} ->
        state = complete_request(state, id, requests)
        send_result(id, {:ok, value})
        {:noreply, state}

      {{:error, {reason, _server}}, id, requests} ->
        state = complete_request(state, id, requests)
        send_result(id, {:exit, reason})
        {:noreply, state}
    end
  end

  defp complete_request(state, id, requests) do
    {_request_id, timer} = Map.fetch!(state.pending, id)
    Process.cancel_timer(timer)
    %{state | requests: requests, pending: Map.delete(state.pending, id)}
  end

  defp delete_request(requests, request_id) do
    requests
    |> :gen_server.reqids_to_list()
    |> Enum.reject(fn {id, _label} -> id == request_id end)
    |> Enum.reduce(:gen_server.reqids_new(), fn {id, label}, acc ->
      :gen_server.reqids_add(id, label, acc)
    end)
  end

  defp send_result(id, {:ok, value}) do
    try do
      reply(id, %{ok: true, value: value})
    rescue
      error in ErlangError ->
        {:unsupported_type, _term} = error.original
        reply(id, %{ok: false, error: %{kind: "unserializable"}})
    end
  end

  defp send_result(id, {:exit, reason}) do
    reply(id, %{ok: false, error: %{kind: "exit", reason: inspect(reason)}})
  end

  defp send_result(id, :timeout) do
    reply(id, %{ok: false, error: %{kind: "timeout"}})
  end

  defp reply(id, payload) do
    Wasm.send(%{_popcorn: %{t: :proxy, id: id, payload: payload}})
  end

  defp whereis(pid) when is_pid(pid), do: if(Process.alive?(pid), do: pid)

  defp whereis(name) when is_binary(name) do
    name
    |> to_atom_or_nil()
    |> GenServer.whereis()
  end

  defp to_atom_or_nil(name) do
    String.to_existing_atom(name)
  rescue
    ArgumentError -> nil
  end
end
