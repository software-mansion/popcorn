defmodule Treeshake.Utils.BeamReader do
  @moduledoc false
  # Reads core erlang from a beam file

  @spec read_core(String.t()) :: {:ok, module(), core_ast :: term()} | :error
  def read_core(beam_path) do
    with :error <- read_core_from_abstract_code(beam_path) do
      read_core_from_custom_debug_info(beam_path)
    end
  end

  defp read_core_from_abstract_code(beam_path) do
    case :beam_lib.chunks(String.to_charlist(beam_path), [:abstract_code]) do
      {:ok, {module, [{:abstract_code, {:raw_abstract_v1, abstract_forms}}]}} ->
        case :compile.noenv_forms(abstract_forms, [:to_core]) do
          {:ok, ^module, core} -> {:ok, module, core}
          {:ok, ^module, core, _warnings} -> {:ok, module, core}
          _other -> :error
        end

      _other ->
        :error
    end
  end

  defp read_core_from_custom_debug_info(beam_path) do
    case :beam_lib.chunks(String.to_charlist(beam_path), [:debug_info]) do
      {:ok, {module, [debug_info: {:debug_info_v1, :core_v1, core}]}} -> {:ok, module, core}
      _other -> :error
    end
  end
end
