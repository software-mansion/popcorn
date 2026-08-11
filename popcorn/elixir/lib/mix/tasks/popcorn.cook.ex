defmodule Mix.Tasks.Popcorn.Cook do
  use Mix.Task

  alias Popcorn.Packager

  @shortdoc "Packages an OTP application for Popcorn"

  @switches [
    app: :string,
    no_app: :boolean,
    extra_app: [:string, :keep],
    out_dir: :string,
    runtime_variant: :string,
    brotli: :boolean,
    strip: :boolean,
    treeshake: :boolean,
    preserved_app: [:string, :keep]
  ]

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

    if preserved_apps != [] && !options[:treeshake] do
      Mix.raise("--preserved-app requires --treeshake")
    end

    packager_options = [
      root_dir: File.cwd!(),
      build_path: Path.join(Mix.Project.build_path(), "lib"),
      out_dir: Keyword.get(options, :out_dir, "priv/static/popcorn"),
      app: entrypoint(options),
      extra_apps: Keyword.get_values(options, :extra_app),
      runtime_variant: options[:runtime_variant],
      brotli: Keyword.get(options, :brotli, false),
      strip: Keyword.get(options, :strip, true),
      treeshake: if(options[:treeshake], do: [preserved_apps: preserved_apps], else: false)
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
end
