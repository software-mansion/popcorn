defmodule ClockLive do
  use LocalLiveView

  def render(assigns) do
    ~H"""
    <p>Current date: {@date}</p>
    """
  end

  def mount(_params, _session, socket) do
    send(self(), :tick)
    {:ok, Phoenix.LiveView.Utils.assign(socket, :date, format_now())}
  end

  def handle_info(:tick, socket) do
    send(self(), :tick)
    :timer.sleep(1000)
    {:noreply, Phoenix.LiveView.Utils.assign(socket, :date, format_now())}
  end

  defp format_now() do
    {{y, m, d}, {h, mm, ss}} = :calendar.local_time()
    date = "#{f(y, 4)}-#{f(m)}-#{f(d)}  #{f(h)}:#{f(mm)}:#{f(ss)}"
  end

  defp f(n, p \\ 2) do
    n
    |> inspect
    |> String.pad_leading(p, "0")
  end
end
