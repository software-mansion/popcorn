defmodule Popdoc do
  @moduledoc """
  ExDoc extension for interactive Elixir code evaluation via Popcorn.
  """

  @bundle_meta_name "popcorn-user-bundle"
  @bundle_asset_dir "assets"
  @user_bundle_name "consumer.avm"
  @assets_dir Path.expand("../assets", __DIR__)
  @assets_target "assets"
  @docs_assets_dir Path.expand("../docs_assets", __DIR__)
  @docs_assets_target "."

  @doc """
  Adds Popdoc's options to ExDoc options.

  ## Options

    * `:popdoc` - Popdoc-specific options.

  ## Popdoc Options

    * `:coi_serviceworker` - when true, injects a root-scoped
      `coi-serviceworker.js` helper for local/debug docs hosting where you
      cannot control response headers. Keep this disabled for production and
      serve `COOP`/`COEP` headers from the real web server instead.
  """
  def config(user_opts \\ []) do
    {popdoc_opts, user_opts} = Keyword.pop(user_opts, :popdoc, [])
    bundle_spec = automatic_bundle_spec()

    user_opts
    |> Keyword.put(:markdown_processor, Popdoc.Markdown)
    |> put_assets(popdoc_opts, bundle_spec)
    |> Keyword.put(
      :before_closing_head_tag,
      &before_closing_head_tag(&1, popdoc_opts, bundle_spec)
    )
    |> Keyword.put(:before_closing_body_tag, &before_closing_body_tag/1)
  end

  defp put_assets(opts, popdoc_opts, bundle_spec) do
    assets =
      opts
      |> Keyword.get(:assets, %{})
      |> Map.put(@assets_dir, @assets_target)
      |> Map.merge(bundle_assets(bundle_spec))

    assets =
      if Keyword.get(popdoc_opts, :coi_serviceworker, false) do
        Map.put(assets, @docs_assets_dir, @docs_assets_target)
      else
        assets
      end

    Keyword.put(opts, :assets, assets)
  end

  defp before_closing_head_tag(:html, popdoc_opts, bundle_spec) do
    [
      ~s(<link rel="stylesheet" href="assets/popdoc.css" />),
      coi_serviceworker_tag(popdoc_opts),
      user_bundle_meta_tags(bundle_spec)
    ]
    |> Enum.join("\n")
  end

  defp before_closing_head_tag(_format, _popdoc_opts, _bundle_spec), do: ""

  defp coi_serviceworker_tag(opts) do
    if Keyword.get(opts, :coi_serviceworker, false) do
      ~s(<script src="coi-serviceworker.js"></script>)
    else
      ""
    end
  end

  defp before_closing_body_tag(:html) do
    ~s(<script type="module" defer src="assets/popdoc.js"></script>)
  end

  defp before_closing_body_tag(_format), do: ""

  defp bundle_assets(nil), do: %{}
  defp bundle_assets(%{asset_mappings: asset_mappings}), do: asset_mappings

  defp user_bundle_meta_tags(nil), do: ""

  defp user_bundle_meta_tags(%{public_paths: public_paths}) do
    Enum.map_join(public_paths, "\n", fn path ->
      ~s(<meta name="#{@bundle_meta_name}" content="#{path}" />)
    end)
  end

  defp automatic_bundle_spec do
    case Mix.Project.config()[:app] do
      nil -> nil
      :popdoc -> nil
      app_name -> build_consumer_bundle(app_name)
    end
  end

  defp build_consumer_bundle(app_name) do
    Mix.shell().info("==> Building #{app_name} bundle files for Popdoc...")
    Mix.Task.run("popdoc.bundle")

    root = user_bundle_root(app_name)
    consumer_bundle_path = Path.join(root, @user_bundle_name)

    if not File.exists?(consumer_bundle_path) do
      Mix.raise("Popdoc expected docs .avm bundle file under #{root}.")
    end

    %{
      asset_mappings: %{root => @bundle_asset_dir},
      public_paths: ["./#{@user_bundle_name}"]
    }
  end

  defp user_bundle_root(app_name) do
    Mix.Project.build_path()
    |> Path.expand()
    |> Path.dirname()
    |> Path.join("popdoc_bundle/output/#{app_name}")
  end
end
