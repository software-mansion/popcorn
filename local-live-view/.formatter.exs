# Used by "mix format"
[
  inputs:
    ["{mix,.formatter}.exs", "{config,lib}/**/*.{ex,exs}"] ++
      Enum.reject(Path.wildcard("test/**/*.{ex,exs}"), &String.starts_with?(&1, "test/e2e/")),
  # The e2e tests' Phoenix app, see test/e2e/README.md
  subdirectories: ["test/e2e"]
]
