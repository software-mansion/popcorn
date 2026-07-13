defmodule DemoApp.Application do
  @moduledoc false
  use Application

  @impl true
  def start(_type, _args) do
    try do
      {:ok, sup} = Supervisor.start_link([HelloPopcorn], strategy: :one_for_one, name: __MODULE__)
      result = DemoApp.Worker.process("hello")
      IO.puts("Worker result: #{result}")
      DemoApp.Behaviour.call_hello(DemoApp.BehaviourImpl)
      DemoApp.ProtocolUser.run()
      {:ok, sup}
    rescue
      e ->
        :erlang.display({e, __STACKTRACE__})
        reraise e, __STACKTRACE__
    end
  end
end
