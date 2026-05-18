defmodule PopdocTest do
  use ExUnit.Case
  doctest Popdoc

  test "greets the world" do
    assert Popdoc.hello() == :world
  end
end
