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
    %{size: size}
    |> html()
    |> mount_at_root()

    listener_refs = add_click_listeners(%{start: "#start", glider: "#glider"})

    # TODO: return list of refs from run_js!/2
    cell_listeners_refs =
      0..(size * size - 1)
      |> Map.new(fn i ->
        x = rem(i, size)
        y = div(i, size)

        {[x, y], ".cell[data-coords='#{x},#{y}']"}
      end)
      |> add_click_listeners()

    {:ok,
     %{
       listener_refs: listener_refs,
       cell_listeners: cell_listeners_refs,
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
        {:wasm_event, :click, _data, "start"} when not is_running(state) ->
          alive = Enum.map(state.alive, fn [x, y] -> {x, y} end)

          {:ok, sup, %{grid_pid: pid}} =
            GridSupervisor.start_simulation(state.size, state.size, alive)

          remove_element("#glider")
          set_text("#start", "Stop simulation")
          timer = start_timer(@tick_speed_ms)

          %{state | sup_pid: sup, grid_pid: pid, timer: timer}

        {:wasm_event, :click, _data, "glider"} when not is_running(state) ->
          new_alive = [
            [2, 0],
            [2, 1],
            [2, 2],
            [1, 2],
            [0, 1]
          ]

          set_alive_cells(new_alive)
          %{state | alive: new_alive}

        {:wasm_event, :click, _data, "start"} ->
          # TODO: allow starting/stopping
          remove_element("#start")
          stop_timer(state.timer)

          %{state | timer: nil, sup_pid: nil, grid_pid: nil}

        {:wasm_cast, "tick"} ->
          new_alive =
            state.grid_pid
            |> Grid.tick()
            |> grid_to_alive_list()

          set_alive_cells(new_alive)
          %{state | alive: new_alive}

        {:wasm_event, :click, _data, [x, y]} when not is_running(state) ->
          if [x, y] in state.alive do
            new_alive = List.delete(state.alive, [x, y])

            set_alive_cells(new_alive)
            %{state | alive: new_alive}
          else
            new_alive = [[x, y] | state.alive]

            set_alive_cells(new_alive)
            %{state | alive: new_alive}
          end

        {:wasm_event, :click, _data, [_x, _y]} ->
          state
      end)

    {:noreply, new_state}
  end

  defp html(%{size: size}) do
    """
    <div class="controls">
      <button id="start">Start simulation</button>
      <button id="glider">Use glider preset</button>
    </div>
    <div class="cell-grid">
      #{build_rows(size)}
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
    |> hd()
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

  defp build_rows(size) do
    0..(size * size - 1)
    |> Enum.map(fn i ->
      ~s|<div class="cell" data-coords="#{rem(i, size)},#{div(i, size)}"></div>|
    end)
    |> Enum.chunk_every(size)
    |> Enum.map(&~s|<div class="cell-row">#{Enum.join(&1)}</div>|)
  end

  def add_click_listeners(selectors) do
    Map.new(selectors, fn {key, selector} ->
      node_ref = query_selector(selector)

      {:ok, [listener_ref]} =
        Wasm.register_event_listener(:click,
          target_node: node_ref,
          event_receiver: @receiver_name,
          custom_data: key
        )

      {key, listener_ref}
    end)
  end

  defp grid_to_alive_list(grid) do
    grid
    |> Grid.to_flat_grid()
    |> Enum.filter(fn {_coords, alive} -> alive end)
    |> Enum.map(fn {{x, y}, true} -> [x, y] end)
  end

  defp set_text(selector, text) do
    Wasm.run_js!(
      """
      ({ args }) => {
        args.node.innerText = args.text;
      }
      """,
      %{node: query_selector(selector), text: text}
    )
  end

  defp remove_element(selector) do
    Wasm.run_js!(
      """
      ({ args }) => {
        args.node.remove();
      }
      """,
      %{node: query_selector(selector)}
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
          const coords = cell.getAttribute("data-coords");
          cell.classList.toggle("cell-alive", alive.has(coords));
        }
      }
      """,
      %{alive_coords: coords}
    )
  end

  defp query_selector(selector) do
    """
    ({ args }) => {
      return [document.querySelector(args.selector)];
    }
    """
    |> Wasm.run_js!(%{selector: selector})
    |> hd()
  end
end
