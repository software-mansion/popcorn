defmodule IexWasmTest do
  use ExUnit.Case
  doctest IexWasm

  test "greets the world" do
    assert IexWasm.hello() == :world
  end
end
