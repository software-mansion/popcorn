defmodule FunctionClauseError do
  # Patch reason: without the patch this module fails to load
  # and aborts AtomVM with message 'Unexpected operand, expected a literal (37)'

  def message(_exception) do
    "no function clause matching"
  end

  def blame(exception, stacktrace) do
    {exception, stacktrace}
  end

  def blame(_exception, _inspect_fun, _fun) do
    ""
  end
end
