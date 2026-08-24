import Config

config :popcorn,
  out_dir: "../priv/static/assets/js/wasm",
  # Temporarily off: the dispatcher now drives the real Phoenix.Socket stack
  # (Phoenix.Socket, Channel.Server, PoolSupervisor), which is wired through
  # dynamic calls the treeshaker cannot see; keeps are not in place yet.
  treeshake: false
