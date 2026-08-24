defmodule LocalLiveView.Dispatcher.View do
  @moduledoc false
  # One mount point's runtime bookkeeping: the create-time incarnation
  # epoch, the channel currently mounted for it (nil between create and
  # join, and in the crash → rejoin window), the messages held while no
  # channel is joined, and the promises of frames awaiting the channel's
  # reply. Everything the view IS — module and identity — travels in the
  # signed session the dispatcher assembles at create, whose only reader is
  # LocalLiveView.Proxy. An entry exists in
  # the registry from create until destroy — a channel crash clears
  # channel_pid, never the entry, and the rejoin's token (re-read off the
  # installed container) carries everything the remount needs.

  defstruct [
    # Create-time incarnation identity, minted by the dispatcher's create
    # clause. Its one job is registration exactness: it rides the signed
    # session and comes back in register_channel, proving which incarnation
    # a mounting channel was created for. Frame staleness is join_ref's job
    # — but a registration token must exist before any join does, at
    # create, when the session is signed.
    :epoch,
    :channel_pid,
    # The dispatcher's own monitor on channel_pid, taken at registration.
    # Its ref discriminates the dispatcher's DOWNs from the socket's own
    # channel monitoring: a DOWN matching this ref is LLV bookkeeping, any
    # other DOWN is Phoenix traffic for the socket.
    :monitor_ref,
    pending: [],
    # Promises of browser frames awaiting this channel's asynchronous reply,
    # by ref — settled with the channel's reply when it arrives, or with an
    # error ack when the channel goes down (see channel_down/1). Living in
    # the entry, they share its lifetime: a destroy discards them with the
    # view, and a reply for a gone view finds no entry.
    replies: %{},
    # True while the view has no live process after having had one — set
    # when the channel goes down (a crash awaiting its rejoin, or a clean
    # stop like redirect/2 that never rejoins), cleared when a channel
    # registers again. Distinguishes "not yet joined" — where queueing is
    # what makes LLV's async construction invisible to callers — from "was
    # joined and died", where some messages must refuse instead (see
    # dispatch/3).
    dead: false
  ]

  alias __MODULE__, as: View

  # The host renders an empty placeholder div ([data-pop-root]) inside each
  # mount point; the LiveView container — data-phx-session/-static/-sticky,
  # with the token signed by LiveView's own sign_nested_session — is
  # rendered here, and the JS side replaces the placeholder with it, then
  # joins it. The channel always mounts LocalLiveView.Proxy; the session
  # (assembled by the dispatcher's create clause, epoch included) carries
  # the actual view module, which the proxy resolves and validates at mount.
  #
  # The fabricated parent socket carries exactly what a connected
  # Static.nested_render reads: the endpoint (token signing), a transport_pid
  # selecting the connected branch (container only, no dead render), a
  # host_uri prune_uri accepts, and an empty assigns container. Sticky
  # renders never touch the parent's pids, router or root_view.
  def render_container(id, session) do
    parent = %Phoenix.LiveView.Socket{
      endpoint: LocalLiveView.Endpoint,
      transport_pid: self(),
      host_uri: :not_mounted_at_router,
      assigns: %{__assigns__: %{}}
    }

    {:safe, iodata} =
      Phoenix.Component.live_render(parent, LocalLiveView.Proxy,
        id: id,
        sticky: true,
        # The container replaces the host-rendered placeholder, so it must
        # carry the marker itself: patchAdoption re-homes the channel of any
        # [data-pop-root] element, and unmount tears its View down.
        container: {:div, data: [pop_root: true]},
        session: session
      )

    IO.iodata_to_binary(iodata)
  end

  # A channel announced itself after a successful mount. The mount ran with
  # the session signed at create time, so flush everything that arrived
  # while no channel was joined — in arrival order, exactly the sequence a
  # live channel would have seen — before anything else reaches the view.
  def register_channel(%View{} = view, pid, monitor_ref) do
    for msg <- Enum.reverse(view.pending), do: send(pid, msg)
    %{view | channel_pid: pid, monitor_ref: monitor_ref, pending: [], dead: false}
  end

  # Send a relayed message to the view's channel, or queue it while none is
  # joined (the create → join round trip, or a crash → rejoin window) for
  # delivery at registration. State-sync messages always queue — replay is
  # exactly what a live channel would have seen. Messages carrying a
  # caller's intent pass `queue: :unless_dead`: they still queue through
  # the construction window (LLV's own asynchrony, invisible to callers),
  # but a dead view refuses them — replaying intent against a view that
  # remounts with fresh state is the hazard LiveView's own pushEvent rejects
  # for, and the refusal is reported back instead of a silent replay.
  def dispatch(view, msg, opts \\ [])

  def dispatch(%View{channel_pid: pid} = view, msg, _opts) when is_pid(pid) do
    send(pid, msg)
    {:ok, view}
  end

  def dispatch(%View{} = view, msg, opts) do
    if view.dead and opts[:queue] == :unless_dead do
      :dead
    else
      # A queued message cannot outlive its view: the queue lives inside
      # this entry, so a destroy discards both together — and the dispatcher
      # drops messages for unknown ids before they get here.
      {:ok, %{view | pending: [msg | view.pending]}}
    end
  end

  # Hold the promise of a browser frame until the channel's reply arrives.
  def put_reply(%View{} = view, ref, promise) do
    %{view | replies: Map.put(view.replies, ref, promise)}
  end

  # The channel replied: hand back the frame's promise (nil if the channel
  # went down first — channel_down already settled it).
  def pop_reply(%View{} = view, ref) do
    {promise, replies} = Map.pop(view.replies, ref)
    {promise, %{view | replies: replies}}
  end

  # The view's channel is gone — crashed (a rejoin follows) or cleanly
  # stopped (no rejoin coming). Settle the frames still awaiting its reply
  # with error acks (instead of letting them time out) and mark the view
  # dead until a channel registers again.
  def channel_down(%View{} = view) do
    for {_ref, promise} <- view.replies do
      Popcorn.Wasm.resolve(%{status: :error, payload: %{reason: "view exited"}}, promise)
    end

    %{view | channel_pid: nil, monitor_ref: nil, dead: true, replies: %{}}
  end
end
