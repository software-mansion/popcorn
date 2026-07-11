{application, test_entrypoint, [
  {description, "Popcorn OTP entrypoint autostart e2e fixture"},
  {vsn, "0.1.0"},
  {modules, [test_entrypoint_app]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {test_entrypoint_app, []}}
]}.
