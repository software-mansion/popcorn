# Serves the e2e app by hand, e.g. to debug a failing test:
#
#     MIX_ENV=test mix run --no-halt test/e2e/server.exs
#     cd test/e2e/playwright && BASE_URL=http://localhost:4905 pnpm test
port = String.to_integer(System.get_env("PORT", "4905"))
LocalLiveView.E2E.Server.build!()
{:ok, pid} = LocalLiveView.E2E.Server.start_link(port)
# Keeps serving after this script's process exits
Process.unlink(pid)
IO.puts("Serving the e2e app at http://localhost:#{port}")
