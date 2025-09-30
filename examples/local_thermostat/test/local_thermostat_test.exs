defmodule LocalThermostatTest do
  use ExUnit.Case
  doctest LocalThermostat

  test "greets the world" do
    assert LocalThermostat.hello() == :world
  end
end
