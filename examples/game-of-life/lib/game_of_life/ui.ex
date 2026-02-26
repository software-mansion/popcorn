defmodule GameOfLife.Ui do
  use GenServer
  import Popcorn.Wasm, only: [is_wasm_message: 1]
  alias Popcorn.Wasm
  alias GameOfLife.Grid
  alias GameOfLife.Supervisor, as: GridSupervisor

  defguardp is_running(state) when is_pid(state.grid_pid)

  @receiver_name :ui
  @tick_speed_ms 300

  def start_link(args) do
    GenServer.start_link(__MODULE__, args, name: @receiver_name)
  end

  @impl GenServer
  def init(%{size: size}) do
    html()
    |> mount_at_root()

    init_grid(size)

    listener_refs =
      register_click_listeners(
        ["#start", "#stop", "#reset", "#glider", ".cell"],
        @receiver_name
      )

    {:ok,
     %{
       listener_refs: listener_refs,
       size: size,
       sup_pid: nil,
       grid_pid: nil,
       alive: [],
       timer: nil
     }}
  end

  @impl GenServer
  def handle_info(raw_msg, state) when is_wasm_message(raw_msg) do
    new_state =
      Wasm.handle_message!(raw_msg, fn
        {:wasm_cast, ["click", "#start", _data]} when not is_running(state) ->
          alive = Enum.map(state.alive, fn [x, y] -> {x, y} end)

          set_element_visiblity(start: false, stop: true, reset: false, examples: false)

          {:ok, sup, %{grid_pid: pid}} =
            GridSupervisor.start_simulation(state.size, state.size, alive)

          timer = start_timer(@tick_speed_ms)

          %{state | sup_pid: sup, grid_pid: pid, timer: timer}

        {:wasm_cast, ["click", "#glider", _data]} when not is_running(state) ->
          new_alive = [
            [2, 0],
            [2, 1],
            [2, 2],
            [1, 2],
            [0, 1]
          ]

          set_alive_cells(new_alive)
          %{state | alive: new_alive}

        {:wasm_cast, ["click", "#stop", _data]} when is_running(state) ->
          set_element_visiblity(start: true, stop: false, reset: true, examples: true)
          stop_timer(state.timer)
          :ok = GridSupervisor.stop_simulation(state.sup_pid)

          %{state | timer: nil, sup_pid: nil, grid_pid: nil}

        {:wasm_cast, ["click", "#reset", _data]} when not is_running(state) ->
          set_alive_cells([])
          %{state | alive: []}

        {:wasm_cast, "tick"} when is_running(state) ->
          new_alive =
            state.grid_pid
            |> Grid.tick()
            |> grid_to_alive_list()

          set_alive_cells(new_alive)
          %{state | alive: new_alive}

        {:wasm_cast, "tick"} ->
          # Stale tick, ignore
          state

        {:wasm_cast, ["click", ".cell", %{"coordsX" => x, "coordsY" => y}]}
        when not is_running(state) ->
          x = String.to_integer(x)
          y = String.to_integer(y)

          if [x, y] in state.alive do
            new_alive = List.delete(state.alive, [x, y])

            set_alive_cells(new_alive)
            %{state | alive: new_alive}
          else
            new_alive = [[x, y] | state.alive]

            set_alive_cells(new_alive)
            %{state | alive: new_alive}
          end
      end)

    {:noreply, new_state}
  end

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

  defp start_timer(ms) do
    # TODO: add unregistration on process exit
    """
    ({ wasm, args }) => {
      return [setInterval(() => wasm.cast(args.receiver, "tick"), args.ms)];
    }
    """
    |> Wasm.run_js!(%{ms: ms, receiver: @receiver_name})
  end

  defp stop_timer(timer_ref) do
    Wasm.run_js!(
      """
      ({ args }) => {
        clearInterval(args.timer);
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
      ({ args }) => {
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
      ({ args }) => {
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
      ({ args }) => {
        document.querySelector("#root").innerHTML = args.html;
      }
      """,
      %{html: html}
    )
  end

  defp set_alive_cells(coords) do
    Wasm.run_js!(
      """
      ({ args }) => {
        const alive = new Set(args.alive_coords.map(([x,y]) => `${x},${y}`));

        for (const cell of document.querySelectorAll(".cell")) {
          const coords = cell.getAttribute("data-coords-x") + ',' + cell.getAttribute("data-coords-y");
          cell.classList.toggle("cell-alive", alive.has(coords));
        }
      }
      """,
      %{alive_coords: coords}
    )
  end

  defp register_click_listeners(selectors, ex_receiver) do
    """
    ({ wasm, args }) => {
      const { selectors, event_name, event_receiver } = args;
      const removeListenersFunctions = [];
      selectors.forEach((selector) => {
        const nodes = document.querySelectorAll(selector);
        const fn = (event) => {
          wasm.cast(event_receiver, ["click", selector, event.target.dataset]);
        };
        nodes.forEach((node) => node.addEventListener(event_name, fn));

        removeListenersFunctions.push(() => {
          nodes.forEach((node) => node.removeEventListener(event_name, fn));
        });
      });

      const key = wasm.nextTrackedObjectKey();
      const cleanupFn = () => {
        removeListenersFunctions.forEach((fn) => fn());
        wasm.cleanupFunctions.delete(key);
      };
      wasm.cleanupFunctions.set(key, cleanupFn);

      return [new TrackedValue({key: key, value: cleanupFn})];
    }
    """
    |> Wasm.run_js!(
      %{
        event_name: "click",
        selectors: selectors,
        event_receiver: ex_receiver
      },
      return: :ref
    )
  end
end
