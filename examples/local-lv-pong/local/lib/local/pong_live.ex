defmodule Local.PongLive do
  use LocalLiveView

  @w 600
  @h 400
  @pw 10
  @ph 70
  @pm 20
  @bs 10
  @win 7

  @default_fps 120
  @fps_options [30, 60, 120, 240]

  # Speeds in pixels per second; multiplied by dt per tick.
  @ps 480
  @bots 300
  @v 300
  @speedup 30
  @maxv 720

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket |> assign(timer: nil, fps: @default_fps, fps_options: @fps_options) |> reset()}
  end

  defp reset(socket) do
    socket
    |> assign(py: (@h - @ph) / 2, by: (@h - @ph) / 2)
    |> assign(up: false, down: false)
    |> assign(ps: 0, bs: 0, status: :waiting)
    |> assign(fps_started_at: nil, fps_ticks: 0, current_fps: 0)
    |> ball(-1)
  end

  defp ball(socket, dir) do
    assign(socket, bx: @w / 2, by_: @h / 2, vx: dir * @v, vy: (:rand.uniform() - 0.5) * @v)
  end

  defp start_timer(socket) do
    tick = div(1000, socket.assigns.fps)
    {:ok, ref} = :timer.send_interval(tick, :tick)
    assign(socket, timer: ref, fps_started_at: nil, fps_ticks: 0)
  end

  defp stop_timer(%{assigns: %{timer: nil}} = socket), do: socket

  defp stop_timer(%{assigns: %{timer: ref}} = socket) do
    :timer.cancel(ref)
    assign(socket, :timer, nil)
  end

  @impl true
  def handle_event("set_fps", %{"fps" => fps}, socket) do
    fps = String.to_integer(fps)
    socket = assign(socket, :fps, fps)

    socket =
      if socket.assigns.timer do
        socket |> stop_timer() |> start_timer()
      else
        socket
      end

    {:noreply, socket}
  end

  def handle_event(e, %{"key" => k}, socket) when e in ["d", "u"] do
    down? = e == "d"

    socket =
      case k do
        x when x in ["ArrowUp", "w", "W"] -> assign(socket, :up, down?)
        x when x in ["ArrowDown", "s", "S"] -> assign(socket, :down, down?)
        " " when down? -> space(socket)
        _ -> socket
      end

    {:noreply, socket}
  end

  defp space(%{assigns: %{status: :playing}} = s),
    do: s |> stop_timer() |> assign(:status, :paused)

  defp space(%{assigns: %{status: :paused}} = s),
    do: s |> start_timer() |> assign(:status, :playing)

  defp space(s),
    do: s |> reset() |> assign(:status, :playing) |> start_timer()

  @impl true
  def handle_info(:tick, %{assigns: %{status: :playing}} = s) do
    {:noreply, s |> measure_fps() |> player() |> bot() |> ball_step() |> win()}
  end

  def handle_info(:tick, s), do: {:noreply, s}

  defp measure_fps(s) do
    now = System.monotonic_time(:millisecond)
    ticks = s.assigns.fps_ticks + 1
    update_freq = 100

    case s.assigns.fps_started_at do
      nil ->
        assign(s, fps_started_at: now, fps_ticks: 0)

      started when now - started >= update_freq ->
        assign(s,
          fps_started_at: now,
          fps_ticks: 0,
          current_fps: round(ticks * 1000 / (now - started))
        )

      _started ->
        assign(s, :fps_ticks, ticks)
    end
  end

  defp dt(s), do: 1 / s.assigns.fps

  defp player(s) do
    step = @ps * dt(s)
    d = if(s.assigns.down, do: step, else: 0) - if(s.assigns.up, do: step, else: 0)
    assign(s, :py, clamp(s.assigns.py + d, 0, @h - @ph))
  end

  defp bot(s) do
    diff = s.assigns.by_ - @ph / 2 - s.assigns.by
    step = @bots * dt(s)

    d =
      cond do
        abs(diff) < step -> diff
        diff > 0 -> step
        true -> -step
      end

    assign(s, :by, clamp(s.assigns.by + d, 0, @h - @ph))
  end

  defp ball_step(s) do
    %{bx: bx, by_: by, vx: vx, vy: vy, py: py, by: byp, ps: ps, bs: bs} = s.assigns
    bx2 = bx + vx * dt(s)
    by2 = by + vy * dt(s)
    {by2, vy} = if by2 < 0 or by2 > @h - @bs, do: {clamp(by2, 0, @h - @bs), -vy}, else: {by2, vy}

    cond do
      bx2 <= @pm + @pw and vx < 0 and by2 + @bs >= py and by2 <= py + @ph ->
        {nvx, nvy} = paddle_bounce(by2, py, vx, vy, 1)
        assign(s, bx: @pm + @pw, by_: by2, vx: nvx, vy: nvy)

      bx2 + @bs >= @w - @pm - @pw and vx > 0 and by2 + @bs >= byp and by2 <= byp + @ph ->
        {nvx, nvy} = paddle_bounce(by2, byp, vx, vy, -1)
        assign(s, bx: @w - @pm - @pw - @bs, by_: by2, vx: nvx, vy: nvy)

      bx2 < -@bs ->
        assign(s, bs: bs + 1) |> ball(-1)

      bx2 > @w ->
        assign(s, ps: ps + 1) |> ball(1)

      true ->
        assign(s, bx: bx2, by_: by2, vy: vy)
    end
  end

  defp win(s) do
    if s.assigns.ps >= @win or s.assigns.bs >= @win do
      s |> stop_timer() |> assign(:status, :over)
    else
      s
    end
  end

  defp clamp(v, lo, hi), do: v |> max(lo) |> min(hi)

  defp paddle_bounce(ball_y, paddle_y, vx, vy, sign) do
    rel = (ball_y + @bs / 2 - (paddle_y + @ph / 2)) / (@ph / 2)
    speed = min(:math.sqrt(vx * vx + vy * vy) + @speedup, @maxv)
    angle = rel * (:math.pi() / 3)
    {sign * speed * :math.cos(angle), speed * :math.sin(angle)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <div phx-window-keydown="d" phx-window-keyup="u" style="font-family:monospace;text-align:center;padding:1em">
      <div><span :if={@status == :over}>{if @ps > @bs, do: "You won!", else: "You lost!"} </span>{@ps} : {@bs}</div>
      <div style="position:relative;width:600px;height:400px;background:#111;margin:1em auto">
        <div style={"position:absolute;left:20px;top:#{@py}px;width:10px;height:70px;background:#fff"}></div>
        <div style={"position:absolute;left:570px;top:#{@by}px;width:10px;height:70px;background:#fff"}></div>
        <div style={"position:absolute;left:#{@bx}px;top:#{@by_}px;width:10px;height:10px;background:#fff"}></div>
      </div>
      <div>
        Press space to play/pause, W/S or ↑/↓ to move
      </div>
      <div style="font-size:0.8em;margin-top:1em;color:#888;">
        <div :if={@status == :playing}>
          FPS: {@current_fps}
        </div>
        <div :if={@status != :playing}>
          Target FPS:
          <%= for opt <- @fps_options do %>
            <button
              type="button"
              phx-click="set_fps"
              phx-value-fps={opt}
              style={"margin:0 2px;padding:2px 8px;background:#{if @fps == opt, do: "#444", else: "#222"};color:#fff;border:1px solid #555;border-radius:3px;cursor:pointer"}
            >{opt}</button>
          <% end %>
        </div>
      </div>
    </div>
    """
  end
end
