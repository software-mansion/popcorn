defmodule FissionLib.Config do
  @moduledoc false
  @config [
            start_module: nil,
            erl_stdlib_beam_paths:
              Path.wildcard("#{:code.lib_dir()}/{compiler,erts,kernel,stdlib}*/**/*.beam"),
            ex_stdlib_beam_paths: Path.wildcard("#{Application.app_dir(:elixir)}/ebin/**/*.beam"),
            out_path: "#{Mix.Project.build_path()}/bundle.avm",
            add_tracing: false
          ]
          |> Map.new(fn {k, v} -> {k, Application.compile_env(:fission_lib, k, v)} end)

  def get(), do: @config
  def get(key), do: Map.fetch!(@config, key)
end
