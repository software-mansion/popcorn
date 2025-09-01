defmodule MyLocalComponent do
  # In Phoenix apps, the line is typically: use MyAppWeb, :html
  use LocalComponent

  def greet(assigns) do
    ~H"""
    <p>Hello, {@name}!</p>
    """
  end
end