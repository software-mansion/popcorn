import Config

config :popcorn,
  out_dir: "../priv/static/assets/js/wasm"

config :popcorn, treeshake: System.get_env("POPCORN_TREESHAKE") |> String.downcase() == "true"
