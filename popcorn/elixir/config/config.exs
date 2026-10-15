import Config

if config_env() == :dev do
  config :makeup_strings,
    languages: [
      typescript: [
        {Popcorn.JS, :run_js, 3},
        {Popcorn.JS, :run_js!, 3}
      ]
    ]
end
