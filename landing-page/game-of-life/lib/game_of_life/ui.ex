defmodule GameOfLife.Ui do
  use GenServer

  alias GameOfLife.{Grid, Pattern}
  alias GameOfLife.Supervisor, as: GridSupervisor
  alias Popcorn.Wasm

  defguardp is_running(state) when is_pid(state.grid_pid)

  @tick_speed_ms 33
  @process_name :game_of_life_ui

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl true
  def init(%{size: size}) do
    {:ok,
     %{
       listener_refs: nil,
       root_id: nil,
       rows: size,
       cols: size,
       sup_pid: nil,
       grid_pid: nil,
       alive: [],
       born: [],
       timer: nil,
       speed: @tick_speed_ms,
       generation: 0
     }}
  end

  @impl true
  def handle_call("mount", from, state) do
    handle_call(["mount", %{"rootId" => "root", "controls" => true}], from, state)
  end

  def handle_call(["mount", options], from, state) do
    state = stop_simulation(state)
    root_id = Map.fetch!(options, "rootId")

    if state.root_id && state.root_id != root_id do
      mount_at_root("", state.root_id)
    end

    state = %{
      state
      | root_id: root_id,
        listener_refs: nil,
        rows: Map.get(options, "rows", state.rows),
        cols: Map.get(options, "cols", state.cols)
    }

    {:noreply, state, {:continue, {:mount, from, Map.get(options, "controls", false)}}}
  end

  def handle_call(["event", event], _from, state) do
    state = handle_event(event, state)
    {:reply, snapshot(state), state}
  end

  def handle_call("snapshot", _from, state), do: {:reply, snapshot(state), state}

  @impl true
  def handle_continue({:mount, from, controls?}, state) do
    state.root_id
    |> html(controls?)
    |> mount_at_root(state.root_id)

    init_grid(state.rows, state.cols, state.root_id)

    listener_refs =
      if controls? do
        register_click_listeners(state.root_id, ["[data-life-action]", ".cell"], self())
      end

    GenServer.reply(from, snapshot(state))
    {:noreply, %{state | listener_refs: listener_refs}}
  end

  @impl true
  def handle_info({:wasm, "tick"}, state), do: {:noreply, handle_tick(state)}

  @impl true
  def handle_cast(["event", event], state), do: {:noreply, handle_event(event, state)}

  defp handle_event("run", state) when not is_running(state) do
    alive = Enum.map(state.alive, fn [x, y] -> {x, y} end)

    {:ok, sup, %{grid_pid: grid}} =
      GridSupervisor.start_simulation(state.rows, state.cols, alive)

    state = %{state | sup_pid: sup, grid_pid: grid, timer: start_timer(state.speed, self())}
    update_controls(state.root_id, true)
    state
  end

  defp handle_event("run", state), do: state
  defp handle_event("pause", state), do: stop_simulation(state)

  defp handle_event("clear", state) do
    state = stop_simulation(state)
    state = %{state | alive: [], born: [], generation: 0}
    set_alive_cells(state)
    state
  end

  defp handle_event("restart", state), do: set_pattern("rpentomino", state)
  defp handle_event(["preset", name], state), do: set_pattern(name, state)

  defp handle_event(["toggle", x, y], state) when not is_running(state) do
    coords = [x, y]

    alive =
      if coords in state.alive do
        List.delete(state.alive, coords)
      else
        [coords | state.alive]
      end

    state = %{state | alive: alive, born: born_cells(alive, state.alive)}
    set_alive_cells(state)
    state
  end

  defp handle_event(["toggle", _, _], state), do: state

  defp handle_event(["speed", speed], state) do
    state = %{state | speed: max(speed, 1)}

    if is_running(state) do
      stop_timer(state.timer)
      %{state | timer: start_timer(state.speed, self())}
    else
      state
    end
  end

  defp handle_event("step", state) when not is_running(state) do
    alive = Enum.map(state.alive, fn [x, y] -> {x, y} end)
    {:ok, sup, %{grid_pid: grid}} = GridSupervisor.start_simulation(state.rows, state.cols, alive)
    next = grid |> Grid.tick() |> grid_to_alive_list()
    :ok = GridSupervisor.stop_simulation(sup)
    update_generation(next, state)
  end

  defp handle_event("step", state), do: state

  defp set_pattern(name, state) do
    state = stop_simulation(state)
    alive = Pattern.centered(name, state.rows, state.cols)
    state = %{state | alive: alive, born: alive, generation: 0}
    set_alive_cells(state)
    state
  end

  defp handle_tick(state) when is_running(state) do
    state.grid_pid
    |> Grid.tick()
    |> grid_to_alive_list()
    |> update_generation(state)
  end

  defp handle_tick(state), do: state

  defp update_generation(alive, state) do
    state = %{
      state
      | alive: alive,
        born: born_cells(alive, state.alive),
        generation: state.generation + 1
    }

    set_alive_cells(state)
    state
  end

  defp stop_simulation(state) when is_running(state) do
    stop_timer(state.timer)
    :ok = GridSupervisor.stop_simulation(state.sup_pid)
    update_controls(state.root_id, false)
    %{state | timer: nil, sup_pid: nil, grid_pid: nil}
  end

  defp stop_simulation(state), do: state

  defp snapshot(state) do
    %{
      "rootId" => state.root_id,
      "rows" => state.rows,
      "cols" => state.cols,
      "alive" => state.alive,
      "born" => state.born,
      "generation" => state.generation,
      "speed" => state.speed,
      "running" => is_running(state)
    }
  end

  defp html(root_id, false), do: ~s(<div id="#{root_id}-grid" class="cell-grid"></div>)

  defp html(root_id, true) do
    """
    <div class="controls">
      <button data-life-action="run">Start simulation</button>
      <button data-life-action="pause" hidden>Stop simulation</button>
      <button data-life-action="clear">Reset</button>
      <button data-life-action="preset:glider">Use glider preset</button>
    </div>
    <div id="#{root_id}-grid" class="cell-grid"></div>
    """
  end

  defp start_timer(ms, receiver) do
    Wasm.run_js!(
      """
      (args, {send}) => {
        const id = setInterval(() => send(args.receiver, "tick"), args.ms);
        const dispose = () => clearInterval(id);
        return new TrackedValue({ dispose }, dispose);
      }
      """,
      %{ms: ms, receiver: receiver}
    )
  end

  defp stop_timer(timer_ref) do
    Wasm.run_js!("(args) => args.timer.dispose()", %{timer: timer_ref})
  end

  defp grid_to_alive_list(grid) do
    grid
    |> Grid.to_flat_grid()
    |> Enum.filter(fn {_coords, alive} -> alive end)
    |> Enum.map(fn {{x, y}, true} -> [x, y] end)
  end

  defp born_cells(alive, previous), do: alive -- previous

  defp update_controls(root_id, running?) do
    Wasm.run_js!(
      """
      (args) => {
        const root = document.getElementById(args.root_id);
        root?.querySelector('[data-life-action="run"]')?.toggleAttribute("hidden", args.running);
        root?.querySelector('[data-life-action="pause"]')?.toggleAttribute("hidden", !args.running);
      }
      """,
      %{root_id: root_id, running: running?}
    )
  end

  defp init_grid(rows, cols, root_id) do
    Wasm.run_js!(
      """
      (args) => {
        const grid = document.getElementById(`${args.root_id}-grid`);
        for (let x = 0; x < args.rows; x++) {
          const row = document.createElement("div");
          row.classList.add("cell-row");
          for (let y = 0; y < args.cols; y++) {
            const cell = document.createElement("button");
            cell.type = "button";
            cell.classList.add("cell");
            cell.dataset.coordsX = x;
            cell.dataset.coordsY = y;
            cell.setAttribute("aria-label", `Toggle cell ${x + 1}, ${y + 1}`);
            row.append(cell);
          }
          grid.append(row);
        }
      }
      """,
      %{rows: rows, cols: cols, root_id: root_id}
    )
  end

  defp mount_at_root(html, root_id) do
    Wasm.run_js!("(args) => document.getElementById(args.root_id).innerHTML = args.html", %{
      html: html,
      root_id: root_id
    })
  end

  defp set_alive_cells(state) do
    Wasm.run_js!(
      """
      (args) => {
        const root = document.getElementById(args.snapshot.rootId);
        const alive = new Set(args.snapshot.alive.map(([x, y]) => `${x},${y}`));
        const born = new Set(args.snapshot.born.map(([x, y]) => `${x},${y}`));
        for (const cell of root.querySelectorAll(".cell")) {
          const coords = `${cell.dataset.coordsX},${cell.dataset.coordsY}`;
          cell.classList.toggle("cell-alive", alive.has(coords));
          cell.classList.toggle("cell-born", born.has(coords));
          cell.setAttribute("aria-pressed", String(alive.has(coords)));
        }
        root.dispatchEvent(new CustomEvent("life:update", { detail: args.snapshot }));
      }
      """,
      %{snapshot: snapshot(state)}
    )
  end

  defp register_click_listeners(root_id, selectors, receiver) do
    Wasm.run_js!(
      """
      (args, {cast}) => {
        const root = document.getElementById(args.root_id);
        const removers = [];
        for (const selector of args.selectors) {
          const nodes = root.querySelectorAll(selector);
          const listener = (event) => {
            const target = event.currentTarget;
            let message = target.dataset.lifeAction;
            if (message?.startsWith("preset:")) message = ["preset", message.slice(7)];
            if (!message) message = ["toggle", Number(target.dataset.coordsX), Number(target.dataset.coordsY)];
            cast(args.receiver, ["event", message]);
          };
          for (const node of nodes) node.addEventListener("click", listener);
          removers.push(() => {
            for (const node of nodes) node.removeEventListener("click", listener);
          });
        }
        const dispose = () => {
          for (const remove of removers) remove();
        };
        return new TrackedValue(null, dispose);
      }
      """,
      %{root_id: root_id, selectors: selectors, receiver: receiver}
    )
  end
end
