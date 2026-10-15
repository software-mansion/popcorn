defmodule Mix.Tasks.Popcorn.Cook do
  use Mix.Task

  alias Popcorn.Packager

  @shortdoc "Packages an OTP application for Popcorn"
  @moduledoc """
  Compiles the current Mix project and packages it for the browser.

  Run the task from the project's directory:

  ```bash
  mix popcorn.cook

  # Customize output directory
  mix popcorn.cook --out-dir priv/static/popcorn
  ```

  The default output directory is `public/out`. Paths are relative to the
  current directory. The task uses the current Mix environment and target.
  For a production build, use `MIX_ENV=prod mix popcorn.cook`.

  **The task deletes the output directory before packaging.** Use a directory
  reserved for generated files. Do not put source files or other assets there.

  ## Applications

  By default, the package includes the current Mix application as its entrypoint.
  Popcorn starts the entrypoint when the browser virtual machine boots.

  The package includes required dependencies from each application's `.app`
  file, including `included_applications`. It excludes optional applications
  unless you select them explicitly. The package always includes the Elixir
  runtime and its required dependencies.

  ```bash
  mix popcorn.cook --app my_app

  # Add extra apps
  mix popcorn.cook --no-app --extra-app eex
  ```

  Use application names, such as `my_app`, rather than module names.
  Extra applications enter the package but do not become entrypoints.
  Use `--no-app` to package the runtime without an entrypoint.

  ## Options

  | Option | Behavior |
  | --- | --- |
  | `--out-dir PATH` | Writes the package to `PATH`. Default: `public/out`. |
  | `--app NAME` | Selects the entrypoint. Default: the current Mix application. |
  | `--no-app` | Selects no entrypoint. Cannot be used with `--app`. |
  | `--extra-app NAME` | Includes an application and its required dependencies. Can be repeated. |
  | `--runtime-variant NAME` | Selects `core` or `crypto` instead of automatic runtime selection. |
  | `--no-strip` | Keeps BEAM debug and documentation chunks. The task strips these by default. |
  | `--exclude-priv PATTERN` | Leaves out `priv` files matching a glob pattern, like `phoenix/priv/static`. Can be repeated. |
  | `--treeshake` | Removes unreachable modules and functions. Experimental. Disabled by default. |
  | `--preserved-app NAME` | Preserves an application's code during tree-shaking. Requires `--treeshake`. Can be repeated. |
  | `--keep ENTRY` | Keeps a module (`MyApp.Plugin`, `:my_module`) or function (`MyApp.Plugin.run/1`), with the code it relies on, during tree-shaking. Requires `--treeshake`. Can be repeated. |
  | `--drop MODULE` | Removes a module during tree-shaking, even if it's reachable. Requires `--treeshake`. Can be repeated. |
  | `--brotli-effort LEVEL` | Selects `standard` (default) or `max`. `max` compresses more slowly. |

  The task selects `crypto` if the package includes `asn1`, `crypto`,
  `public_key`, or `ssl`. Otherwise, it selects `core`.
  An explicit `core` selection fails if an application requires crypto support.

  Tree-shaking can remove code reached only through dynamic calls or evaluation.
  Use `--preserved-app` for such applications:

  ```bash
  mix popcorn.cook --treeshake --extra-app eex --preserved-app eex
  ```

  `--preserved-app` does not add an application to the package.
  Select it through the entrypoint, its dependencies, or `--extra-app` first.
  For particular modules or functions, use `--keep`.

  Some code reached dynamically is kept by default. That includes the
  functions that a library's macros, like `use`, generate in a module
  implementing the library's behaviour, like `__live__/0` of LiveComponents,
  Ecto schemas once they're referenced, and LocalLiveView's views.

  Conversely, some reachable code that's only used to compile, evaluate or
  format code, by a shell, or in distribution, is removed by default:
  Erlang's code evaluator and linter, Elixir's parser and code formatter, the
  shell's line editing, and modules for distribution and a few storage formats.
  Calling a function of a removed module raises `UndefinedFunctionError`, so
  keep the ones an application needs. To evaluate Erlang code, keep
  `:erl_eval` and `:erl_lint`, and to evaluate Elixir code, also preserve
  `elixir`. Evaluated code can only call functions that are packaged.

  ## Output

  The output contains the browser API (`index.mjs`), workers, and WebAssembly
  runtime. The `otp/` directory contains `manifest.json`, the boot script, and
  application archives. Each archive includes the application's `ebin` and
  `priv` directories, if present, without native libraries (`.so`, `.dylib`
  and `.dll` files), which the runtime can't load: the NIFs it supports, like
  `crypto`'s, are linked into it. The archives and the runtime files also have
  `.gz` and `.br` copies.

  Serve the complete output directory. Keep its generated layout.
  Import `Popcorn` from the generated `index.mjs` to use the package.

  Configure the web server with `Cross-Origin-Opener-Policy: same-origin` and
  `Cross-Origin-Embedder-Policy: require-corp`.

  ## Packaging failures

  - If an application is missing, check its name and the current Mix environment
    and target. Fetch project dependencies with `mix deps.get` before cooking.
  - For `unsupported_otp` or `unsupported_boot`, use an OTP installation that
    matches the packaged runtime version.
  """

  @switches [
    app: :string,
    no_app: :boolean,
    extra_app: [:string, :keep],
    out_dir: :string,
    runtime_variant: :string,
    brotli_effort: :string,
    strip: :boolean,
    exclude_priv: [:string, :keep],
    treeshake: :boolean,
    preserved_app: [:string, :keep],
    keep: [:string, :keep],
    drop: [:string, :keep]
  ]

  @doc false
  def switches, do: @switches

  @impl Mix.Task
  def run(argv) do
    Mix.Task.run("compile")
    {options, arguments} = OptionParser.parse!(argv, strict: @switches)

    if arguments != [] do
      Mix.raise("Unexpected arguments: #{Enum.join(arguments, " ")}")
    end

    if options[:app] && options[:no_app] do
      Mix.raise("Use either --app or --no-app")
    end

    preserved_apps = Keyword.get_values(options, :preserved_app)
    keep = options |> Keyword.get_values(:keep) |> Enum.map(&parse_keep/1)
    drop = options |> Keyword.get_values(:drop) |> Enum.map(&parse_module/1)

    for {option, values} <- [preserved_app: preserved_apps, keep: keep, drop: drop],
        values != [] and !options[:treeshake] do
      Mix.raise("--#{String.replace(to_string(option), "_", "-")} requires --treeshake")
    end

    packager_options = [
      root_dir: File.cwd!(),
      build_path: Path.join(Mix.Project.build_path(), "lib"),
      out_dir: Keyword.get(options, :out_dir, "public/out"),
      app: entrypoint(options),
      extra_apps: Keyword.get_values(options, :extra_app),
      runtime_variant: options[:runtime_variant],
      brotli_effort: brotli_effort(options[:brotli_effort]),
      strip: Keyword.get(options, :strip, true),
      exclude_priv: Keyword.get_values(options, :exclude_priv),
      treeshake:
        if(options[:treeshake],
          do: [preserved_apps: preserved_apps, keep: keep, drop: drop],
          else: false
        )
    ]

    case Packager.build(packager_options) do
      {:ok, report} ->
        Mix.shell().info(
          "Cooked #{map_size(report.apps)} applications into #{report.manifestPath}"
        )

      {:error, error} ->
        Mix.raise(Packager.format_error(error))
    end
  end

  defp entrypoint(options) do
    cond do
      options[:no_app] -> nil
      options[:app] -> options[:app]
      true -> Mix.Project.config() |> Keyword.fetch!(:app) |> to_string()
    end
  end

  # `Module`, `Module.fun/arity`, `:module` or `:module.fun/arity`
  defp parse_keep(entry) do
    case Regex.run(~r{^(.+)\.([^./]+)/(\d+)$}, entry) do
      [_, module, fun, arity] ->
        {parse_module(module), String.to_atom(fun), String.to_integer(arity)}

      nil ->
        parse_module(entry)
    end
  end

  defp parse_module(":" <> module), do: String.to_atom(module)
  defp parse_module(module), do: Module.concat([module])

  defp brotli_effort(nil), do: :standard
  defp brotli_effort("standard"), do: :standard
  defp brotli_effort("max"), do: :max

  defp brotli_effort(effort) do
    Mix.raise("Unknown Brotli effort: #{effort}. Expected standard or max")
  end
end
