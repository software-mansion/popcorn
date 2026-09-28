[
  import_deps: [:phoenix],
  plugins: [Phoenix.LiveView.HTMLFormatter],
  locals_without_parens: [live_local: 2],
  inputs: [
    "*.{heex,ex,exs}",
    "support/**/*.{heex,ex,exs}",
    "local/*.exs",
    "local/{config,lib}/**/*.{ex,exs}"
  ]
]
