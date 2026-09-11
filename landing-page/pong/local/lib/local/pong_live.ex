defmodule Local.PongLive do
  use LocalLiveView

  @w 600
  @h 400
  @pw 8
  @ph 88
  @pm 20
  @bs 10
  @win 7

  @fps 60

  # Speeds in pixels per second; multiplied by dt per tick.
  @ps 480
  @bots 300
  @v 300
  @speedup 30
  @maxv 720

  @impl true
  def mount(_params, _session, socket) do
    {:ok, socket |> assign(timer: nil) |> reset()}
  end

  defp reset(socket) do
    socket
    |> assign(py: (@h - @ph) / 2, by: (@h - @ph) / 2)
    |> assign(up: false, down: false)
    |> assign(ps: 0, bs: 0, status: :waiting)
    |> ball(-1)
  end

  defp ball(socket, dir) do
    assign(socket,
      bx: (@w - @bs) / 2,
      by_: (@h - @bs) / 2,
      vx: dir * @v,
      vy: (:rand.uniform() - 0.5) * @v
    )
  end

  defp start_timer(socket) do
    tick = div(1000, @fps)
    {:ok, ref} = :timer.send_interval(tick, :tick)
    assign(socket, :timer, ref)
  end

  defp stop_timer(%{assigns: %{timer: nil}} = socket), do: socket

  defp stop_timer(%{assigns: %{timer: ref}} = socket) do
    :timer.cancel(ref)
    assign(socket, :timer, nil)
  end

  def handle_event("space", _params, socket), do: {:noreply, space(socket)}

  def handle_event("restart", _params, socket), do: {:noreply, socket |> stop_timer() |> reset()}

  def handle_event("move", %{"direction" => direction}, socket) do
    offset = if direction == "up", do: -48, else: 48
    {:noreply, assign(socket, :py, clamp(socket.assigns.py + offset, 0, @h - @ph))}
  end

  def handle_event("move_to", %{"position" => position}, socket) do
    center = String.to_integer(position) / 1000 * @h
    {:noreply, assign(socket, :py, clamp(center - @ph / 2, 0, @h - @ph))}
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
    {:noreply, s |> player() |> bot() |> ball_step() |> win()}
  end

  def handle_info(:tick, s), do: {:noreply, s}

  defp dt, do: 1 / @fps

  defp player(s) do
    step = @ps * dt()
    d = if(s.assigns.down, do: step, else: 0) - if(s.assigns.up, do: step, else: 0)
    assign(s, :py, clamp(s.assigns.py + d, 0, @h - @ph))
  end

  defp bot(s) do
    diff = s.assigns.by_ - @ph / 2 - s.assigns.by
    step = @bots * dt()

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
    bx2 = bx + vx * dt()
    by2 = by + vy * dt()
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
  defp horizontal_percent(value), do: value / @w * 100
  defp vertical_percent(value), do: value / @h * 100
  defp ball_width, do: horizontal_percent(@bs)
  defp paddle_inset, do: horizontal_percent(@pm)
  defp paddle_width, do: horizontal_percent(@pw)
  defp paddle_height, do: @ph / @h * 100

  defp paddle_bounce(ball_y, paddle_y, vx, vy, sign) do
    rel = (ball_y + @bs / 2 - (paddle_y + @ph / 2)) / (@ph / 2)
    speed = min(:math.sqrt(vx * vx + vy * vy) + @speedup, @maxv)
    angle = rel * (:math.pi() / 3)
    {sign * speed * :math.cos(angle), speed * :math.sin(angle)}
  end

  @impl true
  def render(assigns) do
    ~H"""
    <section class="pong-player border-demo bg-cream relative flex h-full min-h-0 w-full flex-col overflow-hidden rounded-md border antialiased">
      <div
        class="pong-court bg-ink focus-visible:outline-orange relative aspect-8/5 w-full flex-none cursor-pointer touch-none overflow-hidden outline-none focus-visible:outline-2 focus-visible:outline-offset-2 @min-demos/demos:h-148.5 @min-demos/demos:aspect-auto"
        phx-keydown="d"
        phx-keyup="u"
        tabindex="0"
        role="application"
        aria-label="Pong court. Press Space to play or pause, and use W, S, or the arrow keys to move."
      >
        <div class="absolute inset-y-3 left-1/2 flex w-1 -translate-x-1/2 flex-col justify-between md:inset-y-7 md:w-1.5">
          <span :for={_ <- 1..14} class="bg-brown-60 h-2.5 w-full md:h-6"></span>
        </div>
        <div
          class="bg-orange absolute"
          style={"left:#{paddle_inset()}%;top:#{vertical_percent(@py)}%;width:#{paddle_width()}%;height:#{paddle_height()}%"}
          data-player-paddle
        >
        </div>
        <div
          class="bg-cream-muted absolute"
          style={"right:#{paddle_inset()}%;top:#{vertical_percent(@by)}%;width:#{paddle_width()}%;height:#{paddle_height()}%"}
        >
        </div>
        <div
          class="bg-cream absolute aspect-square"
          style={"left:#{horizontal_percent(@bx)}%;top:#{vertical_percent(@by_)}%;width:#{ball_width()}%"}
        >
        </div>

        <div
          :if={@status == :playing}
          class="absolute inset-y-0 left-0 z-10 w-1/2 cursor-pointer"
          data-pong-control
        >
        </div>

        <button
          :if={@status == :playing}
          class="sr-only"
          type="button"
          tabindex="-1"
          phx-click="move"
          phx-value-direction="up"
          data-pong-nudge="up"
        >
          Move up
        </button>
        <button
          :if={@status == :playing}
          class="sr-only"
          type="button"
          tabindex="-1"
          phx-click="move"
          phx-value-direction="down"
          data-pong-nudge="down"
        >
          Move down
        </button>
        <button
          :if={@status == :playing}
          class="sr-only"
          type="button"
          tabindex="-1"
          phx-click="move_to"
          phx-value-position="500"
          data-pong-move
        >
          Move paddle
        </button>

        <button
          class="text-cream-muted font-copy absolute top-3 right-3 z-20 flex cursor-pointer items-center gap-1.5 text-xs font-medium md:top-5.5 md:right-5.5 md:gap-2 md:text-sm"
          type="button"
          tabindex="-1"
          phx-click="restart"
        >
          <svg class="size-3.5" viewBox="0 0 16 16" aria-hidden="true">
            <path
              d="M13.5 3v3.5H10M13.2 6.4A5.5 5.5 0 1 0 13.5 9.5"
              fill="none"
              stroke="currentColor"
              stroke-width="1.4"
              stroke-linecap="round"
              stroke-linejoin="round"
            />
          </svg>
          Restart
        </button>

        <button
          :if={@status != :playing}
          class="absolute inset-0 z-10 cursor-pointer"
          type="button"
          tabindex="-1"
          phx-click="space"
          aria-label={status_action(@status, @ps, @bs)}
        >
          <span class="sr-only">{status_action(@status, @ps, @bs)}</span>
        </button>
      </div>

      <footer class="border-demo-soft bg-cream relative flex min-h-0 flex-1 flex-col border-t">
        <div class="flex min-h-24 flex-1 items-end justify-between gap-4 px-4 py-4 md:min-h-23 md:items-center md:px-5.5 md:py-4.5 @min-demos/demos:min-h-36 @min-demos/demos:gap-12 @min-demos/demos:px-5.5 @min-demos/demos:py-6.5">
          <div class="flex items-end gap-6 md:gap-10 @min-demos/demos:gap-12">
            <p class="flex flex-col gap-0.5 md:gap-1">
              <span class={[
                "font-label text-brown-60 text-xs tracking-label uppercase",
                winner_name_class(@status, @ps > @bs)
              ]}>
                You
              </span>
              <strong class={[
                "font-label text-4xl leading-10 font-medium @min-demos/demos:text-5xl @min-demos/demos:leading-12",
                score_class(@status, @ps > @bs)
              ]}>
                {pad_score(@ps)}
              </strong>
            </p>
            <p class="flex flex-col gap-0.5 md:gap-1">
              <span class={[
                "font-label text-brown-60 text-xs tracking-label uppercase",
                winner_name_class(@status, @bs > @ps)
              ]}>
                Beam
              </span>
              <strong class={[
                "font-label text-4xl leading-10 font-medium @min-demos/demos:text-5xl @min-demos/demos:leading-12",
                score_class(@status, @bs > @ps)
              ]}>
                {pad_score(@bs)}
              </strong>
            </p>
          </div>

          <p class="ml-4 flex flex-col items-end gap-0.5 whitespace-nowrap md:ml-6 md:w-28 md:items-start md:gap-1 @min-demos/demos:ml-0 @min-demos/demos:w-auto">
            <span class="font-label text-brown-60 text-xs font-medium tracking-label uppercase">
              Round
            </span>
            <strong class="font-label text-ink text-4xl leading-10 font-medium @min-demos/demos:text-5xl @min-demos/demos:leading-12">
              {round_number(@status, @ps, @bs)}
            </strong>
          </p>

          <section class="hidden min-w-0 flex-1 flex-col gap-1 md:flex">
            <strong class="font-label text-ink text-sm font-medium tracking-wide uppercase">
              Controls
            </strong>
            <p class="font-copy text-brown-70 text-sm leading-5">
              {desktop_controls(@status, @ps, @bs)}
            </p>
          </section>
        </div>

        <section class="border-demo-soft flex flex-col gap-1 border-t px-4 py-3 md:hidden">
          <strong class="font-label text-ink text-xs font-medium tracking-wide uppercase">
            Controls
          </strong>
          <p class="font-copy text-brown-70 text-sm leading-5">
            {mobile_controls(@status, @ps, @bs)}
          </p>
        </section>
      </footer>
    </section>
    """
  end

  defp score_class(_status, true), do: "text-orange"
  defp score_class(_status, false), do: "text-ink"

  defp winner_name_class(:over, true), do: "font-bold"
  defp winner_name_class(_status, _winner?), do: "font-medium"

  defp round_number(:over, player_score, beam_score),
    do: pad_score(player_score + beam_score)

  defp round_number(_status, player_score, beam_score),
    do: pad_score(player_score + beam_score + 1)

  defp desktop_controls(:waiting, _player_score, _beam_score),
    do: "Tap the court or press Space to start. First to 7 wins."

  defp desktop_controls(:playing, _player_score, _beam_score),
    do: "Drag your side or use ↑↓ / W S. Space pauses. First to 7 wins."

  defp desktop_controls(:paused, _player_score, _beam_score),
    do: "Paused. Tap the court or press Space to continue."

  defp desktop_controls(:over, _player_score, _beam_score),
    do: "Tap the court or press Space to play again."

  defp mobile_controls(:waiting, _player_score, _beam_score),
    do: "Tap the court to start. First to 7 wins."

  defp mobile_controls(:playing, _player_score, _beam_score),
    do: "Tap your upper or lower half to move, or drag the paddle. First to 7 wins."

  defp mobile_controls(:paused, _player_score, _beam_score),
    do: "Paused. Tap the court to continue."

  defp mobile_controls(:over, _player_score, _beam_score),
    do: "Tap the court to play again."

  defp status_action(:waiting, _player_score, _beam_score), do: "Start game"
  defp status_action(:paused, _player_score, _beam_score), do: "Continue game"

  defp status_action(:over, _player_score, _beam_score), do: "Play again"

  defp pad_score(score), do: score |> Integer.to_string() |> String.pad_leading(2, "0")
end
