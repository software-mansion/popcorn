defmodule CodeDepsTest do
  use ExUnit.Case
  doctest CodeDeps

  test "greets the world" do
    assert CodeDeps.hello() == :world
  end
end
