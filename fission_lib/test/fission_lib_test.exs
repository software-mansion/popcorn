defmodule FissionLibTest do
  use ExUnit.Case
  doctest FissionLib

  test "greets the world" do
    assert FissionLib.hello() == :world
  end
end
