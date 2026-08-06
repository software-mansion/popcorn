defmodule Popcorn.Wasm.FakeBridge do
  @moduledoc false
  # Stands in for the runtime's `:wasm` module, which only exists in the
  # browser, so `Popcorn.Wasm` can be tested on the host. Raises what the real
  # one raises. See `wasm.erl`.
  #
  # `Popcorn.Fetch`'s JavaScript is not executed; instead a canned response set
  # with `stub_fetch/1` is pushed into the target's mailbox in the shape the
  # real bridge delivers.

  def run_js(_code, %{mode: :timeout}, _opts), do: :erlang.error(:run_js_timeout)
  def run_js(_code, %{mode: :js_error}, _opts), do: :erlang.error({:run_js, "TypeError: x"})
  def run_js(_code, %{mode: :badarg}, _opts), do: :erlang.error(:badarg)
  def run_js(_code, %{mode: :raise}, _opts), do: raise("boom")

  def run_js(_code, %{popcorn_fetch: id} = args, _opts) do
    Process.put(:fetch_request, args)
    Enum.each(events(id), &Kernel.send(args.target, {:wasm, &1}))
    {:wasm_tracked_value, make_ref()}
  end

  def run_js(_code, %{controller: {:wasm_tracked_value, _}}, _opts) do
    Process.put(:fetch_aborted, true)
    nil
  end

  def run_js(_code, args, opts), do: %{"args" => args, "opts" => opts}

  def send(message) do
    Process.put(:sent, message)
    :ok
  end

  @doc """
  Sets what the next fetch replies with.

  Takes `status:`, `headers:` and `chunks:`, or `error:` for a rejected fetch.
  `nil` makes the bridge reply with nothing at all, so the caller times out.
  """
  def stub_fetch(response), do: Process.put(:fetch_response, response)

  @doc "The args the last fetch was started with."
  def fetch_request, do: Process.get(:fetch_request)

  @doc "Whether the last fetch was aborted."
  def fetch_aborted?, do: Process.get(:fetch_aborted, false)

  defp events(id) do
    response = Process.get(:fetch_response) || raise "no fetch stubbed, call stub_fetch/1"
    marker = %{"popcorn_fetch" => id}

    case response do
      :never_replies ->
        []

      %{error: message} ->
        [Map.merge(marker, %{"event" => "error", "error" => message})]

      %{} ->
        head = %{
          "event" => "status",
          "status" => Map.get(response, :status, 200),
          "headers" =>
            Enum.map(Map.get(response, :headers, []), fn {name, value} -> [name, value] end)
        }

        chunks =
          for chunk <- Map.get(response, :chunks, []) do
            %{"event" => "chunk", "data" => Base.encode64(chunk)}
          end

        events =
          if Map.get(response, :done, true) do
            [head | chunks] ++ [%{"event" => "done"}]
          else
            [head | chunks]
          end

        Enum.map(events, &Map.merge(marker, &1))
    end
  end
end
