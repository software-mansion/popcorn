{application, test_entrypoint, [
  {description, "Popcorn OTP entrypoint autostart e2e fixture"},
  {vsn, "0.1.0"},
  {modules, [test_entrypoint_app, test_gen_server]},
  {registered, []},
  {applications, [kernel, stdlib, elixir, popcorn_otp]},
  {mod, {test_entrypoint_app, []}}
]}.
