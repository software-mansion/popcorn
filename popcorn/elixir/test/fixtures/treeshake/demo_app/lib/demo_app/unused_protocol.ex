defprotocol DemoApp.UnusedProtocol do
  def do_nothing(value)
end

defimpl DemoApp.UnusedProtocol, for: DemoApp.Widget do
  def do_nothing(_widget) do
    :ok
  end
end

defimpl DemoApp.UnusedProtocol, for: DemoApp.Gadget do
  def do_nothing(_gadget) do
    :ok
  end
end

defimpl DemoApp.UnusedProtocol, for: Integer do
  def do_nothing(_integer) do
    :ok
  end
end
