import Config

# For running the app by hand, e.g. to debug a failing test:
#
#     mix setup
#     PORT=4905 mix phx.server
#     cd test/playwright && BASE_URL=http://localhost:4905 pnpm test
config :llv_integration, LlvIntegrationWeb.Endpoint,
  http: [ip: {127, 0, 0, 1}, port: String.to_integer(System.get_env("PORT", "4905"))],
  check_origin: false,
  debug_errors: true
