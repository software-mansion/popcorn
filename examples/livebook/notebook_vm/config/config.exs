import Config

config :popcorn,
  # The VMs are Erlang nodes, which Livebook's VM connects
  distribution: true,
  # Mix.install/2 compiles Erlang code that includes OTP's headers, like Mint's
  include_headers: true
