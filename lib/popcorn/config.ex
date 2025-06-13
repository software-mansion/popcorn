defmodule Popcorn.Config do
  @moduledoc false
  @defaults %{
    erl_stdlib_beam_paths:
      Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam"),
    # [:elixir, :eex, :iex, :logger]
    ex_stdlib_beam_paths:
      [:elixir, :eex, :iex]
      |> Enum.map(&"#{Application.app_dir(&1)}/ebin/**/*.beam")
      |> Enum.flat_map(&Path.wildcard/1),
    out_dir: nil,
    add_tracing: false,
    runtime_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}
  }

  def get(key) do
    Application.get_env(:popcorn, key, Map.fetch!(@defaults, key))
  end

  defmacro compile(keys) do
    defaults = Map.take(@defaults, List.wrap(keys)) |> Enum.to_list()

    quote do
      Map.new(unquote(defaults), fn {key, default} ->
        {key, Application.compile_env(:popcorn, key, default)}
      end)
    end
  end
end
