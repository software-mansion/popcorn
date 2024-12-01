defmodule IExWASM do
  def start() do
    Console.print("Starting application...\n")
    Process.register(self(), :main)
    loop()
  end

  defp loop() do
    receive do
      {:emscripten, {:call, promise, message}} ->
        {type, code} = type(message)

        code
        |> :erlang.binary_to_list()
        |> eval(type)
        |> resolve(promise)
    end

    loop()
  end

  defp type("eval:" <> code), do: {:eval, code}
  defp type("module:" <> code), do: {:module, code}

  defp eval(string, :module) do
    compile_opts = [
      :deterministic,
      :return_errors,
      :compressed,
      :no_spawn_compiler_process,
      :no_docs
    ]

    parse_form = fn form_tok ->
      {:ok, form} = :erl_parse.parse_form(form_tok)
      form
    end

    try do
      {:ok, tokens, _} = :erl_scan.string(string)

      {:ok, module, module_bin} =
        tokens
        |> split_forms()
        |> Enum.map(parse_form)
        |> :compile.noenv_forms(compile_opts)

      :code.load_binary(module, "nofile", module_bin)
    catch
      error -> {:error, error}
    end
  end

  defp eval(string, :eval) do
    try do
      {:ok, tokens, _} = :erl_scan.string(string)
      {:ok, exprs} = :erl_parse.parse_exprs(tokens)
      {:value, value, _} = :erl_eval.exprs(exprs, [])
      value
    catch
      error -> {:error, error}
    end
  end

  defp resolve(term, promise) do
    value = :io_lib.format(~c"~p", [term])

    :emscripten.promise_resolve(promise, value)
  end

  defp split_forms(forms) do
    split_on_dots = fn
      {:dot, _} = f, {current, forms} ->
        new_form = Enum.reverse([f | current])
        {[], [new_form | forms]}

      f, {current, forms} ->
        {[f | current], forms}
    end

    {[], split} = Enum.reduce(forms, {[], []}, split_on_dots)
    Enum.reverse(split)
  end
end
