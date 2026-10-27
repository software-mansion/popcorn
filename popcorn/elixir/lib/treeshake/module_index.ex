defmodule Treeshake.ModuleIndex do
  @moduledoc false

  # Builds a %{module => info} map using reader, analyzer and privates_resolver.
  # Injects hardcoded information.

  @type t :: %{module() => PrivatesResolver.module_info()}

  def build(opts, hardcoded) do
    ignore_modules = for {:mod, module} <- opts.ignore, do: module
    dropped_modules = for {:mod, module} <- opts.drop, do: module
    skip_modules = MapSet.new(ignore_modules ++ dropped_modules)
    ignore_funs = MapSet.new(for {:mfa, mfa} <- opts.ignore, do: mfa)

    opts.ebin_files
    |> Enum.filter(&(Path.extname(&1) == ".beam"))
    |> Enum.reject(&(beam_module(&1) in skip_modules))
    |> process_async(&build_entry(&1, hardcoded, ignore_funs))
  end

  defp build_entry(path, hardcoded, ignore_funs) do
    {module, core} = Treeshake.Utils.BeamReader.read_core!(path)
    definitions = Treeshake.Utils.BeamReader.read_elixir_definitions(path)
    info = Treeshake.Utils.BeamAnalyzer.analyze(module, core, definitions)

    info =
      info
      |> Map.update!(:functions, &add_hardcoded_calls(&1, module, hardcoded))
      |> Map.update!(:behaviour_impls, &add_hardcoded_behaviour_impls(&1, module, hardcoded))
      |> Map.update!(:functions, &ignore_funs(&1, module, ignore_funs))
      |> Treeshake.Utils.PrivatesResolver.resolve()

    {info.module, info}
  end

  # Hardcoded calls are made by a function, or by all functions of a module
  defp add_hardcoded_calls(functions, module, hardcoded) do
    module_calls = Map.get(hardcoded.calls, module, [])

    Enum.map(functions, fn function ->
      calls = Map.get(hardcoded.calls, {module, function.name, function.arity}, [])
      %{function | calls: module_calls ++ calls ++ function.calls}
    end)
  end

  defp add_hardcoded_behaviour_impls(behaviour_impls, module, hardcoded) do
    case Map.fetch(hardcoded.behaviour_impls, module) do
      {:ok, hardcoded_impls} -> hardcoded_impls ++ behaviour_impls
      :error -> behaviour_impls
    end
  end

  defp ignore_funs(functions, module, ignore_funs) do
    Enum.map(functions, fn fun_info ->
      if {module, fun_info.name, fun_info.arity} in ignore_funs do
        %{fun_info | calls: [], potential_modules: []}
      else
        fun_info
      end
    end)
  end

  defp process_async(enum, fun) do
    enum
    |> Task.async_stream(fun, ordered: false, timeout: 30_000)
    |> Map.new(fn {:ok, result} -> result end)
  end

  defp beam_module(beam_path) do
    beam_path |> Path.basename(".beam") |> String.to_atom()
  end
end
