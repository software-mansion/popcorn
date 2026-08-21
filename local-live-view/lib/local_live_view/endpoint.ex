defmodule LocalLiveView.Endpoint do
  @moduledoc false
  # The minimal endpoint surface Phoenix.LiveView.Channel consults when
  # hosting local views inside the WASM runtime.
  #
  # The signing secret is a constant on purpose: session tokens are signed by
  # the dispatcher and verified by the channel inside the same browser-local
  # runtime — they never cross a trust boundary, the token machinery is only
  # exercised because the stock channel requires it.
  #
  # hibernate_after is :infinity because :proc_lib.hibernate is untested on
  # AtomVM; an idle local view must not gamble on it.
  def config(:live_view), do: [signing_salt: "local-live-view", hibernate_after: :infinity]
  def config(:secret_key_base), do: "local-live-view-browser-only-secret-key-base-000000000000000000"
  def config(_key), do: nil
end
