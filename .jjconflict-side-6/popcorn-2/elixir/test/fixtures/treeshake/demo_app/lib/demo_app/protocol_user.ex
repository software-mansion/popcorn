defmodule DemoApp.ProtocolUser do
  @moduledoc false
  # Called from Application.start/2 — reachable.
  # Calls DemoApp.Formatter.format/1 with both a built-in type (Integer)
  # and a struct type (Widget), so both implementations must be reachable.
  def run() do
    DemoApp.Formatter.format(42)
    DemoApp.Formatter.format(%DemoApp.Widget{name: "test"})
  end
end
