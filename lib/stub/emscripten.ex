defmodule :emscripten do
  @spec from_remote_object(reference(), :key | :value) :: term()
  def from_remote_object(_resource, _return) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec promise_resolve(reference(), binary()) :: :ok
  def promise_resolve(_promise, _serialized_term) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec promise_reject(reference(), binary()) :: :ok
  def promise_reject(_promise, _serialized_term) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec run_remote_object_fn_script(binary(), [{:main_thread, boolean()}, {:async, boolean()}]) ::
          {:ok, reference()}
  def run_remote_object_fn_script(_code, _opts) do
    :erlang.nif_error(:nif_not_loaded)
  end
end
