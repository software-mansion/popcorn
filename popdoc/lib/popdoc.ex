defmodule Popdoc do
  @moduledoc """
  ExDoc extension for interactive Elixir code evaluation via Popcorn.
  """

  @assets_dir Path.expand("../assets", __DIR__)
  @assets_target "assets"
  @docs_assets_dir Path.expand("../docs_assets", __DIR__)
  @docs_assets_target "."

  @doc """
  Adds Popdoc's options to ExDoc options.

  ## Options

    * `:coi_serviceworker` - when true, injects a root-scoped
      `coi-serviceworker.js` helper for local/debug docs hosting where you
      cannot control response headers. Keep this disabled for production and
      serve `COOP`/`COEP` headers from the real web server instead.
  """
  def config(user_opts \\ []) do
    # TODO: check if we have better naming for the coi option
    user_opts
    |> Keyword.put(:markdown_processor, Popdoc.Markdown)
    |> put_assets()
    |> Keyword.put(:before_closing_head_tag, &before_closing_head_tag(&1, user_opts))
    |> Keyword.put(:before_closing_body_tag, &before_closing_body_tag/1)
  end

  defp put_assets(opts) do
    assets =
      opts
      |> Keyword.get(:assets, %{})
      |> Map.put(@assets_dir, @assets_target)

    assets =
      if Keyword.get(opts, :coi_serviceworker, false) do
        Map.put(assets, @docs_assets_dir, @docs_assets_target)
      else
        assets
      end

    Keyword.put(opts, :assets, assets)
  end

  defp before_closing_head_tag(:html, opts) do
    [
      ~s(<link rel="stylesheet" href="assets/popdoc.css" />),
      coi_serviceworker_tag(opts)
    ]
    |> Enum.join("\n")
  end

  defp before_closing_head_tag(_format, _opts), do: ""

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
end
