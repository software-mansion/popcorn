defmodule Treeshake.ModuleIndex do
  @moduledoc false

  # Builds a %{module => info} map using reader, analyzer and privates_resolver.
  # Injects hardcoded information.

  @type t :: %{module() => PrivatesResolver.module_info()}

  def build(opts, hardcoded) do
    {ignore_modules, ignore_funs} = Enum.split_with(opts.ignore, &is_atom/1)
    skip_modules = MapSet.new(ignore_modules ++ opts.drop)
    ignore_funs = MapSet.new(ignore_funs)

    opts.ebin_files
    |> Enum.filter(&(Path.extname(&1) == ".beam"))
    |> Enum.reject(&(beam_module(&1) in skip_modules))
    |> process_async(&build_entry(&1, hardcoded, ignore_funs))
  end

  defp build_entry(path, hardcoded, ignore_funs) do
    {:ok, module, core} = Treeshake.Utils.BeamReader.read_core(path)
    info = Treeshake.Utils.BeamAnalyzer.analyze(module, core)

    info =
      info
      |> Map.update!(:functions, &add_hardcoded_calls(&1, module, hardcoded))
      |> Map.update!(:behaviour_impls, &add_hardcoded_behaviour_impls(&1, module, hardcoded))
      |> Map.update!(:functions, &ignore_funs(&1, module, ignore_funs))
      |> Treeshake.Utils.PrivatesResolver.resolve()

    {info.module, info}
  end

  defp add_hardcoded_calls(functions, module, hardcoded) do
    case Map.fetch(hardcoded.calls, module) do
      {:ok, calls} -> Enum.map(functions, &%{&1 | calls: calls ++ &1.calls})
      :error -> functions
    end
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
