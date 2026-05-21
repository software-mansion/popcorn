defmodule Popdoc do
  @moduledoc """
  ExDoc extension for interactive Elixir code evaluation via Popcorn.
  """

  @assets_dir Path.expand("../assets", __DIR__)
  @assets_target "assets"

  @doc """
  Adds Popdoc's options to ExDoc options.
  """
  def config(user_opts \\ []) do
    user_opts
    |> Keyword.put(:markdown_processor, Popdoc.Markdown)
    |> Keyword.update(
      :assets,
      %{@assets_dir => @assets_target},
      &Map.put(&1, @assets_dir, @assets_target)
    )
    |> Keyword.put(:before_closing_head_tag, &before_closing_head_tag/1)
    |> Keyword.put(:before_closing_body_tag, &before_closing_body_tag/1)
  end

  defp before_closing_head_tag(:html) do
    ~s(<link rel="stylesheet" href="assets/popdoc.css" />)
  end

  defp before_closing_head_tag(_format), do: ""

  defp before_closing_body_tag(:html) do
    ~s(<script type="module" defer src="assets/popdoc.js"></script>)
  end

  defp before_closing_body_tag(_format), do: ""
end
