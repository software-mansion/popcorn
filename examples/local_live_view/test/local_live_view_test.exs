defmodule LocalLiveViewTest do
  use ExUnit.Case
  doctest LocalLiveView

  test "greets the world" do
    assert LocalLiveView.hello() == :world
  end
end
