defmodule Popcorn.Config do
  @moduledoc false
  @defaults %{
    start_module: nil,
    erl_stdlib_beam_paths:
      Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam"),
    ex_stdlib_beam_paths:
      Path.wildcard("#{Application.app_dir(:elixir)}/ebin/**/*.beam") ++
        Path.wildcard("#{Application.app_dir(:eex)}/ebin/**/*.beam"),
    out_dir: nil,
    add_tracing: false,
    runtime_source: {:git, "git@github.com:software-mansion-labs/FissionVM.git"}
  }

  defmacro get(keys) do
    defaults = Map.take(@defaults, List.wrap(keys)) |> Enum.to_list()

    quote do
      Map.new(unquote(defaults), fn {key, default} ->
        {key, Application.compile_env(:popcorn, key, default)}
      end)
    end
  end
end
