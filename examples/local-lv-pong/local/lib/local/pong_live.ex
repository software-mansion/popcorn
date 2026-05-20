defmodule Local.PongLive do
  use LocalLiveView

  @w 600
  @h 400
  @pw 10
  @ph 70
  @pm 20
  @bs 10
  @win 7

  @fps 120
  @tick div(1000, @fps)
  @dt 1 / @fps

  # Speeds in pixels per second; multiplied by @dt per tick.
  @ps 480
  @bots 300
  @v 300
  @speedup 18
  @maxv 720

  @impl true
  def mount(_params, _session, socket) do
    {:ok, reset(socket)}
  end

  defp reset(socket) do
    socket
    |> assign(py: (@h - @ph) / 2, by: (@h - @ph) / 2)
    |> assign(up: false, down: false)
    |> assign(ps: 0, bs: 0, status: :waiting)
    |> ball(-1)
  end

  defp ball(socket, dir) do
    assign(socket, bx: @w / 2, by_: @h / 2, vx: dir * @v, vy: (:rand.uniform() - 0.5) * @v)
  end

  @impl true
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

  defp space(%{assigns: %{status: :playing}} = s), do: assign(s, :status, :paused)

  defp space(%{assigns: %{status: :paused}} = s) do
    Process.send_after(self(), :tick, @tick)
    assign(s, :status, :playing)
  end

  defp space(s) do
    Process.send_after(self(), :tick, @tick)
    reset(s) |> assign(:status, :playing)
  end

  @impl true
  def handle_info(:tick, %{assigns: %{status: :playing}} = s) do
    Process.send_after(self(), :tick, @tick)
    {:noreply, s |> player() |> bot() |> ball_step() |> win()}
  end

  def handle_info(:tick, s), do: {:noreply, s}

  defp player(s) do
    step = @ps * @dt
    d = if(s.assigns.down, do: step, else: 0) - if(s.assigns.up, do: step, else: 0)
    assign(s, :py, clamp(s.assigns.py + d, 0, @h - @ph))
  end

  defp bot(s) do
    diff = s.assigns.by_ - @ph / 2 - s.assigns.by
    step = @bots * @dt

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
    bx2 = bx + vx * @dt
    by2 = by + vy * @dt
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
    cond do
      s.assigns.ps >= @win -> assign(s, status: :over)
      s.assigns.bs >= @win -> assign(s, status: :over)
      true -> s
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
      <div>{@ps} : {@bs}</div>
      <div style="position:relative;width:600px;height:400px;background:#111;margin:1em auto">
        <div style={"position:absolute;left:20px;top:#{@py}px;width:10px;height:70px;background:#fff"}></div>
        <div style={"position:absolute;left:570px;top:#{@by}px;width:10px;height:70px;background:#fff"}></div>
        <div style={"position:absolute;left:#{@bx}px;top:#{@by_}px;width:10px;height:10px;background:#fff"}></div>
      </div>
      <div>
        Press space to play/pause, W/S or ↑/↓ to move
      </div>
    </div>
    """
  end
end
