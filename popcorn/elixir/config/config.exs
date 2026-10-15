import Config

if config_env() == :dev do
  config :makeup_strings,
    languages: [
      typescript: [
        {Popcorn.JS, :run, 3},
        {Popcorn.JS, :run!, 3}
      ]
    ]
end
