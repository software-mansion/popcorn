defmodule Popcorn.Config do
  @moduledoc false
  @defaults %{
    extra_apps: [],
    out_dir: nil,
    add_tracing: false,
    runtime: [
      {:url, "https://github.com/software-mansion-labs/FissionVM/releases/latest/download/",
       target: :wasm},
      {:path, "popcorn_runtime_source/artifacts/$target"}
    ]
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
