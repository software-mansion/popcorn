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

  @doc """
  Divides `numerator` by each value in `denominators`, raising on zero.
  """
  def divide_all(numerator, denominators) do
    Enum.map(denominators, fn d -> safe_div(numerator, d) end)
  end

  defp safe_div(n, d) do
    check_denominator(d)
    div(n, d)
  end

  defp check_denominator(0), do: raise(ArgumentError, "denominator must be non-zero")
  defp check_denominator(_), do: :ok
end
