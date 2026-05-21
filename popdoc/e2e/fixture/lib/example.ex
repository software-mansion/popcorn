defmodule Example do
  @moduledoc """
  Small helper module for the Popdoc fixture.
  """

  @doc """
  Returns a short greeting.
  """
  def hello do
    :world
  end

  @doc """
  Adds two integers.
  """
  def add(left, right) do
    left + right
  end
end
