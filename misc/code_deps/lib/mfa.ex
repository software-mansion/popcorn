defmodule MFA do
  def print({m, f, a}) do
    "#{inspect(m)}.#{f}/#{a}"
  end

  def elixir_module?({m, _f, _a}) do
    m |> Atom.to_string() |> String.starts_with?("Elixir")
  end
end
