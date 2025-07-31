defmodule ElixirTourTest do
  use ExUnit.Case
  doctest ElixirTour

  test "greets the world" do
    assert ElixirTour.hello() == :world
  end
end
