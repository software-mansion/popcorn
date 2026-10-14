defmodule GameOfLife.Ui do
  use GenServer

  alias Popcorn.Wasm
  alias GameOfLife.Grid
  alias GameOfLife.Supervisor, as: GridSupervisor

  defguardp is_running(state) when is_pid(state.grid_pid)

  @tick_speed_ms 300
  @process_name :game_of_life_ui

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @process_name)
  end

  @impl GenServer
  def init(%{size: size}) do
    {:ok,
     %{
       listener_refs: nil,
       size: size,
       sup_pid: nil,
       grid_pid: nil,
       alive: [],
       timer: nil
     }}
  end

  @impl GenServer
  def handle_call("mount", from, state) do
    {:noreply, state, {:continue, {:mount, from}}}
  end

  @impl GenServer
  def handle_continue({:mount, from}, state) do
    html()
    |> mount_at_root()

    init_grid(state.size)

    listener_refs =
      register_click_listeners(["#start", "#stop", "#reset", "#glider", ".cell"], self())

    GenServer.reply(from, "mounted")
    {:noreply, %{state | listener_refs: listener_refs}}
  end

  @impl GenServer
  def handle_info({:wasm, "tick"}, state) do
    {:noreply, handle_tick(state)}
  end

  @impl GenServer
  def handle_cast(event, state) do
    {:noreply, handle_dom_event(event, state)}
  end

  defp handle_dom_event(["click", "#start", _data], state) when not is_running(state) do
    alive = Enum.map(state.alive, fn [x, y] -> {x, y} end)

    set_element_visiblity(start: false, stop: true, reset: false, examples: false)

    {:ok, sup, %{grid_pid: pid}} =
      GridSupervisor.start_simulation(state.size, state.size, alive)

    timer = start_timer(@tick_speed_ms, self())

    %{state | sup_pid: sup, grid_pid: pid, timer: timer}
  end

  defp handle_dom_event(["click", "#glider", _data], state) when not is_running(state) do
    new_alive = [
      [2, 0],
      [2, 1],
      [2, 2],
      [1, 2],
      [0, 1]
    ]

    set_alive_cells(new_alive)
    %{state | alive: new_alive}
  end

  defp handle_dom_event(["click", "#stop", _data], state) when is_running(state) do
    set_element_visiblity(start: true, stop: false, reset: true, examples: true)
    stop_timer(state.timer)
    :ok = GridSupervisor.stop_simulation(state.sup_pid)

    %{state | timer: nil, sup_pid: nil, grid_pid: nil}
  end

  defp handle_dom_event(["click", "#reset", _data], state) when not is_running(state) do
    set_alive_cells([])
    %{state | alive: []}
  end

  defp handle_dom_event(["click", ".cell", %{"coordsX" => x, "coordsY" => y}], state)
       when not is_running(state) do
    x = String.to_integer(x)
    y = String.to_integer(y)

    new_alive =
      if [x, y] in state.alive do
        List.delete(state.alive, [x, y])
      else
        [[x, y] | state.alive]
      end

    set_alive_cells(new_alive)
    %{state | alive: new_alive}
  end

  defp handle_tick(state) when is_running(state) do
    new_alive =
      state.grid_pid
      |> Grid.tick()
      |> grid_to_alive_list()

    set_alive_cells(new_alive)
    %{state | alive: new_alive}
  end

  # Stale tick, ignore
  defp handle_tick(state), do: state

  defp html() do
    """
    <div class="controls">
      <button id="start">Start simulation</button>
      <button id="stop" hidden>Stop simulation</button>
      <button id="reset">Reset</button>
      <div id="examples">
        <button id="glider">Use glider preset</button>
      </div>
    </div>
    <div id="grid-root" class="cell-grid">
    </div>
    """
  end

  defp start_timer(ms, receiver) do
    Wasm.run_js!(
      """
      (args, {send}) => {
        const id = setInterval(
          () => send(args.receiver, "tick"),
          args.ms,
        );

        let disposed = false;
        const dispose = () => {
          if (disposed) return;
          disposed = true;
          clearInterval(id);
        };

        return new TrackedValue({ dispose }, dispose);
      }
      """,
      %{ms: ms, receiver: receiver}
    )
  end

  defp stop_timer(timer_ref) do
    Wasm.run_js!(
      """
      (args) => {
        args.timer.dispose();
      }
      """,
      %{timer: timer_ref}
    )
  end

  defp grid_to_alive_list(grid) do
    grid
    |> Grid.to_flat_grid()
    |> Enum.filter(fn {_coords, alive} -> alive end)
    |> Enum.map(fn {{x, y}, true} -> [x, y] end)
  end

  defp set_element_visiblity(visibility_by_id) do
    visibility =
      Enum.map(visibility_by_id, fn
        {id, visible?} -> ["#" <> to_string(id), visible?]
      end)

    Wasm.run_js!(
      """
      (args) => {
        for (let [id, isVisible] of args.visibility) {
          const el = document.querySelector(id);
          if (isVisible) {
            el.removeAttribute('hidden');
          } else {
            el.setAttribute('hidden', '');
          }
        }
      }
      """,
      %{visibility: visibility}
    )
  end

  defp init_grid(size) do
    Wasm.run_js!(
      """
      (args) => {
        const root = document.querySelector("#grid-root");
        for (let x = 0; x < args.size; x++) {
          const row = document.createElement("div");
          row.classList.add("cell-row");
          for (let y = 0; y < args.size; y++) {
            const cell = document.createElement("div");
            cell.classList.add("cell");
            cell.setAttribute("data-coords-x", x);
            cell.setAttribute("data-coords-y", y);
            row.append(cell);
          }
          root.append(row);
        }
      }
      """,
      %{size: size}
    )
  end

  defp mount_at_root(html) do
    Wasm.run_js!(
      """
      (args) => {
        document.querySelector("#root").innerHTML = args.html;
      }
      """,
      %{html: html}
    )
  end

  defp set_alive_cells(coords) do
    Wasm.run_js!(
      """
      (args) => {
        const alive = new Set(args.alive_coords.map(([x, y]) => `${x},${y}`));

        for (const cell of document.querySelectorAll(".cell")) {
          const coords = cell.getAttribute("data-coords-x") + ',' + cell.getAttribute("data-coords-y");
          cell.classList.toggle("cell-alive", alive.has(coords));
        }
      }
      """,
      %{alive_coords: coords}
    )
  end

  # Returns a tracked handle; listeners are removed when the handle is
  # garbage-collected on the BEAM side, so it's kept in the state.
  defp register_click_listeners(selectors, receiver) do
    Wasm.run_js!(
      """
      (args, {cast}) => {
        const { selectors, event_name, event_receiver } = args;
        const removeListenersFunctions = [];
        selectors.forEach((selector) => {
          const nodes = document.querySelectorAll(selector);
          const fn = (event) => {
            cast(event_receiver, ["click", selector, { ...event.target.dataset }]);
          };
          nodes.forEach((node) => node.addEventListener(event_name, fn));

          removeListenersFunctions.push(() => {
            nodes.forEach((node) => node.removeEventListener(event_name, fn));
          });
        });

        const cleanup = () => removeListenersFunctions.forEach((fn) => fn());
        return new TrackedValue(null, cleanup);
      }
      """,
      %{
        event_name: "click",
        selectors: selectors,
        event_receiver: receiver
      }
    )
  end
end
