defmodule LivebookWasm.Runtime do
  @moduledoc """
  A Livebook runtime in a VM of its own, which the page boots for each
  notebook, as Livebook's standalone runtime starts an Elixir OS process.

  The VMs are Erlang nodes that the page connects. A runtime VM boots the same
  package as Livebook's, but only sets up `Mix.install/2`, and Livebook loads
  its runtime's code into it.
  """

  defstruct [:node, :server_pid]

  @type t :: %__MODULE__{node: node() | nil, server_pid: pid() | nil}

  # How long the page has to boot the VM
  @boot_timeout 60_000

  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc false
  def __connect__(runtime) do
    caller = self()

    {:ok, pid} =
      DynamicSupervisor.start_child(
        Livebook.RuntimeSupervisor,
        {Task, fn -> do_connect(runtime, caller) end}
      )

    pid
  end

  # The VMs' hosts differ from Livebook's, so that Livebook doesn't share file
  # paths with them: each VM has its own file system
  defp do_connect(runtime, caller) do
    node = :"runtime_#{System.unique_integer([:positive])}@popcorn"
    Popcorn.JS.send(%{livebook_runtime: "start", node: node, cookie: Node.get_cookie()})

    case await_node(node, System.monotonic_time(:millisecond) + @boot_timeout) do
      :ok ->
        server_pid =
          Livebook.Runtime.ErlDist.initialize(node,
            runtime_server_opts: [
              extra_smart_cell_definitions: Livebook.Runtime.Definitions.smart_cell_definitions()
            ]
          )

        send(
          caller,
          {:runtime_connect_done, self(), {:ok, %{runtime | node: node, server_pid: server_pid}}}
        )

      {:error, message} ->
        stop_vm(node)
        send(caller, {:runtime_connect_done, self(), {:error, message}})
    end
  end

  defp await_node(node, deadline) do
    cond do
      :net_kernel.hidden_connect_node(node) and Node.ping(node) == :pong ->
        :ok

      System.monotonic_time(:millisecond) > deadline ->
        {:error, "the runtime VM didn't start"}

      true ->
        Process.sleep(100)
        await_node(node, deadline)
    end
  end

  @doc false
  def stop_vm(node) do
    Popcorn.JS.send(%{livebook_runtime: "stop", node: node})
  end
end

defimpl Livebook.Runtime, for: LivebookWasm.Runtime do
  alias Livebook.Runtime.ErlDist.RuntimeServer

  def describe(runtime) do
    [{"Type", "Popcorn VM"}, {"Node name", Atom.to_string(runtime.node)}]
  end

  def connect(runtime), do: LivebookWasm.Runtime.__connect__(runtime)

  def take_ownership(runtime, opts) do
    RuntimeServer.attach(runtime.server_pid, self(), opts)
    Process.monitor(runtime.server_pid)
  end

  def disconnect(runtime) do
    RuntimeServer.stop(runtime.server_pid)
    Node.disconnect(runtime.node)
    LivebookWasm.Runtime.stop_vm(runtime.node)
    :ok
  end

  # A fresh VM, as reconnecting starts over
  def duplicate(_runtime), do: LivebookWasm.Runtime.new()

  def evaluate_code(runtime, language, code, locator, parent_locators, opts) do
    RuntimeServer.evaluate_code(
      runtime.server_pid,
      language,
      code,
      locator,
      parent_locators,
      opts
    )
  end

  def forget_evaluation(runtime, locator) do
    RuntimeServer.forget_evaluation(runtime.server_pid, locator)
  end

  def drop_container(runtime, container_ref) do
    RuntimeServer.drop_container(runtime.server_pid, container_ref)
  end

  def handle_intellisense(runtime, send_to, language, request, parent_locators, node) do
    RuntimeServer.handle_intellisense(
      runtime.server_pid,
      send_to,
      language,
      request,
      parent_locators,
      node
    )
  end

  def read_file(runtime, path), do: RuntimeServer.read_file(runtime.server_pid, path)

  def relabel_file(runtime, file_id, new_file_id) do
    RuntimeServer.relabel_file(runtime.server_pid, file_id, new_file_id)
  end

  def transfer_file(runtime, path, file_id, callback) do
    RuntimeServer.transfer_file(runtime.server_pid, path, file_id, callback)
  end

  def revoke_file(runtime, file_id), do: RuntimeServer.revoke_file(runtime.server_pid, file_id)

  def start_smart_cell(runtime, kind, ref, attrs, parent_locators) do
    RuntimeServer.start_smart_cell(runtime.server_pid, kind, ref, attrs, parent_locators)
  end

  def set_smart_cell_parent_locators(runtime, ref, parent_locators) do
    RuntimeServer.set_smart_cell_parent_locators(runtime.server_pid, ref, parent_locators)
  end

  def stop_smart_cell(runtime, ref), do: RuntimeServer.stop_smart_cell(runtime.server_pid, ref)

  # Notebooks install packages from Hex with Mix.install/2, as in the standalone runtime
  def supports_dependencies?(_runtime), do: true

  def has_dependencies?(runtime, dependencies) do
    RuntimeServer.has_dependencies?(runtime.server_pid, dependencies)
  end

  def packages_source(_runtime), do: :hex

  def put_system_envs(runtime, envs), do: RuntimeServer.put_system_envs(runtime.server_pid, envs)

  def delete_system_envs(runtime, names) do
    RuntimeServer.delete_system_envs(runtime.server_pid, names)
  end

  def restore_transient_state(runtime, transient_state) do
    RuntimeServer.restore_transient_state(runtime.server_pid, transient_state)
  end

  def register_clients(runtime, clients) do
    RuntimeServer.register_clients(runtime.server_pid, clients)
  end

  def unregister_clients(runtime, client_ids) do
    RuntimeServer.unregister_clients(runtime.server_pid, client_ids)
  end

  def fetch_proxy_handler_spec(runtime) do
    RuntimeServer.fetch_proxy_handler_spec(runtime.server_pid)
  end

  def disconnect_node(runtime, node), do: RuntimeServer.disconnect_node(runtime.server_pid, node)
end
