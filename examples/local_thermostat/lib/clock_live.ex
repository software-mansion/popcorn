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
    Process.sleep(1000)
    {:noreply, Phoenix.LiveView.Utils.assign(socket, :date, format_now())}
  end

  defp format_now() do
    NaiveDateTime.local_now()
    |> Calendar.strftime("%c")
  end

  defp f(n, p \\ 2) do
    n
    |> inspect
    |> String.pad_leading(p, "0")
  end
end
