defmodule :emscripten do
  @moduledoc false

  @type remote_object_ref() :: binary()
  @type tracked_object_ref() :: binary()

  @spec from_remote_object(remote_object_ref(), :key | :value) :: term()
  def from_remote_object(_ref, _return) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec run_remote_object_fn_script(binary(), [{:main_thread, boolean()}, {:async, boolean()}]) ::
          {:ok, remote_object_ref()}
  def run_remote_object_fn_script(_code, _opts) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec get_tracked([tracked_object_ref()], :key | :value) :: [
          {:error, :badvalue} | {:error, :badkey} | {:ok, binary()}
        ]
  def get_tracked(_refs, _type) do
    :erlang.nif_error(:nif_not_loaded)
  end

  @spec run_script_tracked(binary()) :: {:ok, [tracked_object_ref()]} | {:error, term()}
  def run_script_tracked(_code) do
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
end
