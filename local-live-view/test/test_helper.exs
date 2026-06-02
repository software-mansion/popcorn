ExUnit.start()
# E2E tests spin up a real Phoenix project + browser — slow, run them explicitly:
#   mix test --only e2e
ExUnit.configure(exclude: [:e2e])
