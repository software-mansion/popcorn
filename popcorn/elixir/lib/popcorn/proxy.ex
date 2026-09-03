defmodule Popcorn.Proxy do
  @default_name :popcorn_proxy
  @default_timeout_ms 5_000

  @moduledoc """
  Connects JavaScript calls and casts to GenServers in the VM.

  To receive them, add `Popcorn.Proxy` to your supervision tree.

  Notes:
  - One proxy handles concurrent requests to multiple GenServers.
  - Targets can be registered names or PID handles from the same VM.

  See `Popcorn.Wasm` for value conversions.

  ## Example

  Define a GenServer that accepts JavaScript requests:

  ```elixir
  defmodule MyApp.Counter do
    use GenServer

    def start_link(_opts), do: GenServer.start_link(__MODULE__, 0, name: :counter)

    @impl true
    def init(count), do: {:ok, count}

    @impl true
    def handle_call(["add", n], _from, count) do
      {:reply, count + n, count + n}
    end

    @impl true
    def handle_cast("reset", _count), do: {:noreply, 0}
  end
  ```

  Add both processes to your application supervisor:

  ```elixir
  children = [MyApp.Counter, Popcorn.Proxy]
  Supervisor.start_link(children, strategy: :one_for_one)
  ```

  After the VM boots, call the counter from JavaScript:

  ```js
  const result = await popcorn.genserver.call("counter", ["add", 1]);
  if (!result.ok) throw result.error;
  console.log(result.data); // 1

  await popcorn.genserver.cast("counter", "reset");
  ```

  ## Calls and casts

  Calls wait for a reply, including deferred replies from `GenServer.reply/2`.
  Calls report missing processes, server exits, replies that cannot be serialized, and timeouts.
  The `timeoutMs` JavaScript option defaults to `#{@default_timeout_ms}`. A timeout does not cancel the GenServer's work.

  Casts are fire-and-forget.

  ## Custom proxy names

  Use `{Popcorn.Proxy, name: :ui_proxy}` in the supervision tree.
  Select it in JavaScript with the option `{proxy: "ui_proxy"}`.
  For multiple proxies under one supervisor, assign distinct child IDs with `Supervisor.child_spec/2`.
  """

  use GenServer

  alias Popcorn.Wasm

  @doc """
  Starts a proxy linked to the current process.

  ## Options

  - `:name` - the registered name. Defaults to `#{inspect(@default_name)}`. JavaScript selects this name with its `proxy` option.
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
