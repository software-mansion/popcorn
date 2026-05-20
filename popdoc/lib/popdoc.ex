defmodule Popdoc do
  @moduledoc """
  ExDoc extension for interactive Elixir code evaluation via Popcorn.
  """

  @doc """
  Adds Popdoc's options to ExDoc options.
  """
  def config(user_opts \\ []) do
    Keyword.put(user_opts, :markdown_processor, Popdoc.Markdown)
  end
end
