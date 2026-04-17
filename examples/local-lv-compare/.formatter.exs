[
  import_deps: [:phoenix, :phoenix_live_view],
  plugins: [Phoenix.LiveView.HTMLFormatter],
  inputs: ["*.{heex,ex,exs}", "{config,lib,test}/**/*.{heex,ex,exs}"],
  locals_without_parens: [attr: 2, attr: 3, slot: 1, slot: 2, slot: 3]
]
