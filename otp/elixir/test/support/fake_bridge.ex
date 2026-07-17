defmodule Popcorn.Wasm.FakeBridge do
  @moduledoc false
  # Stands in for the runtime's `:wasm` module, which only exists in the
  # browser, so `Popcorn.Wasm` can be tested on the host. Raises what the real
  # one raises. See `wasm.erl`.

  def run_js(_code, %{mode: :timeout}, _opts), do: :erlang.error(:run_js_timeout)
  def run_js(_code, %{mode: :js_error}, _opts), do: :erlang.error({:run_js, "TypeError: x"})
  def run_js(_code, %{mode: :badarg}, _opts), do: :erlang.error(:badarg)
  def run_js(_code, %{mode: :raise}, _opts), do: raise("boom")
  def run_js(_code, args, opts), do: %{"args" => args, "opts" => opts}

  def send(message) do
    Process.put(:sent, message)
    :ok
  end
end
