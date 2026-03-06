# Stub: telemetry is not available on AtomVM, so we provide no-op implementations

module = :telemetry
File.rm(:code.which(module))
:code.purge(module)

defmodule module do
  def execute(_event, _measurements, _metadata \\ %{}), do: :ok

  def span(_event, _metadata, fun) do
<<<<<<< HEAD:local-live-view/priv/static/stubs/telemetry_stub.ex
=======
    IO.puts("STUBBED!")
>>>>>>> 6bff854 (Include deps with runtime: false in the bundle, adjust local liveview):local-live-view/lib/stubs/telemetry_stub.ex
    {result, _updated_metadata} = fun.()
    result
  end

  def attach(_handler_id, _event, _fun, _config), do: :ok
  def attach_many(_handler_id, _events, _fun, _config), do: :ok
  def detach(_handler_id), do: :ok
  def list_handlers(_event), do: []
end
