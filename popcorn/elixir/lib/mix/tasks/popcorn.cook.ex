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
    treeshake: [enable: true]
  ```

  | Setting | Behavior |
  | --- | --- |
  | `app` | Entrypoint application. Default: the current Mix application. Set `nil` for no entrypoint. |
  | `extra_apps` | Additional applications to include with their required dependencies. Default: `[]`. |
  | `runtime_variant` | Selects `:core` or `:crypto` instead of automatic runtime selection. |
  | `strip` | Removes BEAM debug and documentation chunks. Default: `true`. |
  | `exclude_priv` | Leaves out `priv` files matching glob patterns, which are relative to `priv`: `{app, pattern}` in one application's, like `{:phoenix, "templates"}`, and a bare pattern in every application's, like `"static/*.map"`. A matched directory is left out with its contents. Default: `[]`. |
  | `treeshake` | Removes unreachable modules and functions. Experimental. Set `[enable: true]` to enable. See [Tree-shaking](#module-tree-shaking) below for details & more options. Default: `[enable: false]`. |
  | `brotli_effort` | Selects `:standard` (default) or `:max`. `:max` compresses more slowly. |

  Use application names, such as `:my_app`, rather than module names.
  Runtime variants also accept strings.
  Popcorn starts the entrypoint when the browser virtual machine boots.
  Extra applications enter the package but do not become entrypoints.

  The package includes **all** compiled applications in the current
  environment's build directory, including `runtime: false` and transitive Mix
  dependencies. Mix libraries with no `.app` file (`app: false`) are not
  packaged. The package always includes the Elixir runtime, Popcorn's
  application, which boots the VM, and their required dependencies.

  The package includes the applications' config, in the boot script, which the
  browser can read. `runtime.exs` is not evaluated.

  > #### Secrets {: .warning }
  >
  > Don't store secrets in your Popcorn app's config. It will be readable by the browser.

  The task selects `crypto` VM variant if the package includes `asn1`, `crypto`,
  `public_key`, or `ssl`. Otherwise, it selects `core`.
  An explicit `core` selection fails if an application requires crypto support.

  ### Tree-shaking

  > #### Experimental feature {: .warning }
  >
  > Tree-shaking is experimental. There may be breaking changes to its behaviour and
  > configuration, even in minor releases.

  Tree-shaking finds code that nothing calls and removes it from the bundle.
  It can significantly reduce bundle size, which is important for web apps.

  Due to the dynamic nature of Elixir, it's not possible to detect unused code with
  full certainty. In particular, if some code is only called through dynamic calls
  (like `apply/3` on a module known at runtime), it's possible that tree-shaking
  removes it.

  You can use the `keep` option to prevent given piece of code from being removed,
  along with all the code it depends on. `keep` accepts a keyword list with the following
  entries:
  - `mod: module` - keeps all public functions of a module,
  - `mfa: {module, fun, arity}` - keeps a given function,
  - `behaviour_impls: behaviour` - keeps all modules implementing a given behaviour

  For example:

  ```elixir
  config :popcorn,
    treeshake: [enable: true, keep: [mod: MyApp.Plugin, mfa: {MyApp.Codec, :decode, 1}]]
  ```

  `keep` can be also used to work-around a bug in the tree-shaker. In such case, please
  open an issue in Popcorn.

  Tree-shaking doesn't work for use cases involving code compilation and evaluation.
  The compiler and evaluator code is dropped by default.

  ## Output

  The output contains the browser API (`index.mjs`), workers, and WebAssembly
  runtime. The `otp/` directory contains `manifest.json` and three archives:
  `stdlib.<hash>.tar`, `app.<hash>.tar`, and `deps.<hash>.tar`. They contain
  selected OTP/Elixir built-ins, user applications, and remaining dependencies
  respectively. All output artifacts also have `.gz` and `.br` variants.

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
      app: Application.get_env(:popcorn, :app, Keyword.fetch!(Mix.Project.config(), :app)),
      extra_apps: Application.get_env(:popcorn, :extra_apps, []),
      runtime_variant: runtime_variant(Application.get_env(:popcorn, :runtime_variant)),
      brotli_effort: Application.get_env(:popcorn, :brotli_effort, :standard),
      strip: Application.get_env(:popcorn, :strip, true),
      exclude_priv: Application.get_env(:popcorn, :exclude_priv, []),
      config: project_config(),
      treeshake: Application.get_env(:popcorn, :treeshake, enable: false)
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

  defp project_config do
    with path when is_binary(path) <- Mix.Project.config()[:config_path],
         true <- File.regular?(path) do
      Config.Reader.read!(path, env: Mix.env(), target: Mix.target())
    else
      _not_found -> []
    end
  end

  defp runtime_variant(nil), do: nil
  defp runtime_variant(variant), do: to_string(variant)
end
