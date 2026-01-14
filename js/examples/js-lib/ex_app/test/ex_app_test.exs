defmodule ExAppTest do
  use ExUnit.Case
  doctest ExApp

  test "greets the world" do
    assert ExApp.hello() == :world
  end
end
