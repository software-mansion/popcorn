defmodule MakeupStrings.Application do
  @moduledoc false

  use Application

  alias Makeup.Registry

  @impl true
  def start(_type, _args) do
    Registry.register_lexer(MakeupStrings.ElixirLexer,
      names: ["elixir", "iex"],
      extensions: ["ex", "exs"]
    )

    Supervisor.start_link([], strategy: :one_for_one, name: MakeupStrings.Supervisor)
  end
end
