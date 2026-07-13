defprotocol DemoApp.Formatter do
  def format(value)
end

defimpl DemoApp.Formatter, for: Integer do
  def format(value), do: "int:#{value}"
end

defimpl DemoApp.Formatter, for: DemoApp.Widget do
  def format(value), do: "widget:#{value.name}"
end

defimpl DemoApp.Formatter, for: DemoApp.Gadget do
  # DemoApp.Gadget is never referenced — this implementation should be dead.
  def format(value), do: "gadget:#{value.value}"
end
