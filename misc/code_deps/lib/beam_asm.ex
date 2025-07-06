defmodule BeamASM do
  def parse(file) do
    {:beam_file, module, exp_funs, _vsn, _meta, body} = :beam_disasm.file(~c"#{file}")
    exp_funs = Enum.map(exp_funs, fn {f, a, _line} -> {module, f, a} end)
    {module, exp_funs, body}
  end

  def analyze({module, _exp_funs, asm}) do
    Enum.flat_map(asm, fn
      {:function, function, arity, _line, body} ->
        [{{module, function, arity}, get_calls(body)}]

      _other ->
        []
    end)
  end

  defp get_calls(asm) do
    asm
    |> Enum.filter(&is_tuple/1)
    |> Enum.flat_map(fn tuple ->
      type = tuple |> elem(0) |> Atom.to_string()

      case type do
        "call_ext" <> _ ->
          {:extfunc, m, f, a} = elem(tuple, 2)
          [{m, f, a}]

        "call_fun" <> _ ->
          []

        "call" <> _ ->
          {m, f, a} = elem(tuple, 2)
          [{m, f, a}]

        _other ->
          []
      end
    end)
  end
end
