import Config

config :phoenix, :json_library, Jason

config :phx_wasm, PhxWasm.Endpoint,
  url: [host: "localhost"],
  secret_key_base: String.duplicate("popcorn-spike-secret-", 4),
  render_errors: [formats: [html: PhxWasm.ErrorHTML], layout: false],
  pubsub_server: PhxWasm.PubSub,
  live_view: [signing_salt: "spike-lv-salt"],
  server: false
