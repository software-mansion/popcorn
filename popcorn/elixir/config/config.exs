import Config

if config_env() == :dev do
  config :makeup_strings,
    languages: [
      typescript: [
        {Popcorn.Wasm, :run_js, 3},
        {Popcorn.Wasm, :run_js!, 3}
      ]
    ]
end
