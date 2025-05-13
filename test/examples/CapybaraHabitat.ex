defmodule CapybaraHabitat do
  def start do
    {:ok, _} = Environment.start_link(10)
    {:ok, _} = Feeder.start_link()
  end

  def stop do
    GenServer.cast(:environment, :stop)
    GenServer.cast(:feeder, :stop)
  end

  def create_capybara(name, activity) do
    {:ok, capybara} = Capybara.start_link(name, activity)
    GenServer.cast(:environment, {:new_capybara, capybara})
  end
end

defmodule Environment do
  use GenServer

  def start_link(initial_food) do
    GenServer.start_link(__MODULE__, initial_food, name: :environment)
  end

  def init(food) do
    Process.send_after(self(), :info, 10_000)
    {:ok, %{food: food, capybaras: []}}
  end

  def handle_cast({:add_food, amount}, %{food: food} = state) do
    {:noreply, %{state | food: food + amount}}
  end

  def handle_cast({:request_food, capybara_pid}, %{food: food} = state) when food > 0 do
    GenServer.cast(capybara_pid, :food)
    {:noreply, %{state | food: food - 1}}
  end

  def handle_cast({:request_food, capybara_pid}, state) do
    GenServer.cast(capybara_pid, :empty)
    {:noreply, state}
  end

  def handle_cast({:new_capybara, pid}, %{capybaras: cap} = state) do
    {:noreply, %{state | capybaras: [pid | cap]}}
  end

  def handle_cast(:stop, %{capybaras: cap}) do
    kill_capybaras(cap)
    {:stop, :normal, :ok}
  end

  def handle_info(:info, %{food: food, capybaras: cap} = state) when food > 0 do
    IO.puts(
      "Environment is stable containing #{length(cap)} happy capybaras, with #{food} remaining food."
    )

    Process.send_after(self(), :info, 10_000)
    {:noreply, state}
  end

  def handle_info(:info, %{capybaras: cap} = state) do
    IO.puts("Environment containing #{length(cap)} hungry capybaras, without remaining food.")
    Process.send_after(self(), :info, 10_000)
    {:noreply, state}
  end

  defp kill_capybaras([]) do
    :ok
  end

  defp kill_capybaras([cap | rest]) do
    GenServer.cast(cap, :kill)
    kill_capybaras(rest)
  end
end

defmodule Feeder do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, [], name: :feeder)
  end

  def init(_) do
    schedule_food()
    {:ok, []}
  end

  defp schedule_food() do
    Process.send_after(self(), :add_food, 30_000)
  end

  def handle_cast(:stop, _) do
    {:stop, :normal, :ok}
  end

  def handle_info(:add_food, state) do
    GenServer.cast(:environment, {:add_food, Enum.random(1..10)})
    schedule_food()
    {:noreply, state}
  end
end

defmodule Capybara do
  use GenServer

  def start_link(name, activity) do
    GenServer.start_link(__MODULE__, %{name: name, activity: activity})
  end

  def init(state) do
    schedule_food_request()
    {:ok, state}
  end

  defp schedule_food_request do
    Process.send_after(self(), :request_food, 2000)
  end

  def handle_info(:request_food, %{name: name} = state) do
    IO.puts("#{name} is now hungry.")
    Process.sleep(2000)
    GenServer.cast(:environment, {:request_food, self()})
    {:noreply, state}
  end

  def handle_cast(:kill, _state) do
    {:stop, :normal, :ok}
  end

  def handle_cast(:food, %{name: name, activity: activity} = state) do
    IO.puts("#{name} is now eating.")
    Process.sleep(2000)
    Process.send_after(self(), :request_food, 5000)
    IO.puts("#{name} is now #{activity}.")
    {:noreply, state}
  end

  def handle_cast(:empty, %{name: name, activity: _activity} = state) do
    IO.puts("#{name} is now REALLY HUNGRY.")
    Process.sleep(2000)
    Process.send_after(self(), :request_food, 5000)
    {:noreply, state}
  end
end
