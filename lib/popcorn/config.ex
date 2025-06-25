defmodule Popcorn.Config do
  @moduledoc false
  @defaults %{
    start_module: nil,
    erl_stdlib_beam_paths:
      Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam"),
    ex_stdlib_beam_paths:
      [:elixir, :eex, :logger, :iex]
      |> Enum.flat_map(&Path.wildcard("#{Application.app_dir(&1)}/ebin/**/*.beam")),
    out_dir: nil,
    add_tracing: false,
    runtime: {:url, "https://popcorn.swmansion.com/simple_repl/wasm/", target: :wasm},
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
