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

  ## Configuration

  Set cooking options in your project's `config/config.exs`. The same settings
  apply when cooking through a JavaScript bundler plugin.

  ```elixir
  import Config

  config :popcorn,
    extra_apps: [:eex],
    treeshake: [preserved_apps: [:eex]]
  ```

  | Setting | Behavior |
  | --- | --- |
  | `app` | Entrypoint application. Default: the current Mix application. Set `nil` for no entrypoint. |
  | `extra_apps` | Additional applications to include with their required dependencies. Default: `[]`. |
  | `runtime_variant` | Selects `:core` or `:crypto` instead of automatic runtime selection. |
  | `strip` | Removes BEAM debug and documentation chunks. Default: `true`. |
  | `treeshake` | Removes unreachable modules and functions. Experimental. Default: `false`. Set `[preserved_apps: [...]]` to enable. |
  | `brotli_effort` | Selects `:standard` (default) or `:max`. `:max` compresses more slowly. |

  Use application names, such as `:my_app`, rather than module names.
  Application names and runtime variants also accept strings.
  Popcorn starts the entrypoint when the browser virtual machine boots.
  Extra applications enter the package but do not become entrypoints.

  The package includes required dependencies from each application's `.app`
  file, including `included_applications`. It excludes optional applications
  unless you select them explicitly. The package always includes the Elixir
  runtime and its required dependencies.

  The task selects `crypto` if the package includes `asn1`, `crypto`,
  `public_key`, or `ssl`. Otherwise, it selects `core`.
  An explicit `core` selection fails if an application requires crypto support.

  Tree-shaking can remove code reached only through dynamic calls or evaluation.
  Use `preserved_apps` for such applications. Preservation does not add an
  application to the package; select it through the entrypoint, its dependencies,
  or `extra_apps` first. Use `treeshake: []` to enable without preserving apps.

  ## Output

  The output contains the browser API (`index.mjs`), workers, and WebAssembly
  runtime. The `otp/` directory contains `manifest.json`, the boot script, and
  application archives. Each archive includes the application's `ebin` and
  `priv` directories, if present. Archives also have `.gz` and `.br` copies.

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

  @switches [out_dir: :string]

  @doc false
  def switches, do: @switches

  @impl Mix.Task
  def run(argv) do
    {options, arguments} = OptionParser.parse!(argv, strict: @switches)

    if arguments != [] do
      Mix.raise("Unexpected arguments: #{Enum.join(arguments, " ")}")
    end

    Mix.Task.run("compile")

    packager_options = [
      root_dir: File.cwd!(),
      build_path: Path.join(Mix.Project.build_path(), "lib"),
      out_dir: Keyword.get(options, :out_dir, "public/out"),
      app:
        app_name(Application.get_env(:popcorn, :app, Keyword.fetch!(Mix.Project.config(), :app))),
      extra_apps: Enum.map(Application.get_env(:popcorn, :extra_apps, []), &to_string/1),
      runtime_variant: app_name(Application.get_env(:popcorn, :runtime_variant)),
      brotli_effort: Application.get_env(:popcorn, :brotli_effort, :standard),
      strip: Application.get_env(:popcorn, :strip, true),
      treeshake: treeshake_options(Application.get_env(:popcorn, :treeshake, false))
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

  defp app_name(nil), do: nil
  defp app_name(name), do: to_string(name)

  defp treeshake_options(false), do: false

  defp treeshake_options(options) when is_list(options) do
    Keyword.update(options, :preserved_apps, [], &Enum.map(&1, fn app -> to_string(app) end))
  end
end
