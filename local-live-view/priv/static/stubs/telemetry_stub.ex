# Stub: telemetry is not available on AtomVM, so we provide no-op implementations
defmodule :telemetry do
  def execute(_event, _measurements, _metadata \\ %{}), do: :ok

  def span(_event, _metadata, fun) do
    {result, _updated_metadata} = fun.()
    result
  end

  def attach(_handler_id, _event, _fun, _config), do: :ok
  def attach_many(_handler_id, _events, _fun, _config), do: :ok
  def detach(_handler_id), do: :ok
  def list_handlers(_event), do: []
end
