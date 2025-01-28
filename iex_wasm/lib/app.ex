defmodule App do
  def start() do
    Console.print("Starting interpreter...\n")
    Process.register(self(), :main)
    loop()
  end

  defp loop() do
    receive do
      {:emscripten, {:call, promise, message}} ->
        {type, code} = type(message)

        code
        |> eval(type)
        |> resolve(promise)
    end

    loop()
  end

  defp type("eval:elixir:" <> code), do: {:elixir, code}
  defp type("eval:erlang:" <> code), do: {:erlang, code}
  defp type("eval_module:erlang:" <> code), do: {{:module, :erlang}, code}

  defp eval(code, :elixir) do
    unless Process.whereis(:elixir_config) do
      :elixir.start([], [])
    end

    code
    |> Code.eval_string([], __ENV__)
    |> elem(0)
  end

  defp eval(code, {:module, :erlang}) do
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

    with code = :erlang.binary_to_list(code),
         {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, module, module_bin} <-
           tokens
           |> split_forms()
           |> Enum.map(parse_form)
           |> :compile.noenv_forms(compile_opts),
         {:module, _module} <- :code.load_binary(module, ~c"nofile", module_bin) do
      :ok
    end
  rescue
    error -> {:error, error, __STACKTRACE__}
  end

  defp eval(code, :erlang) do
    with code = :erlang.binary_to_list(code),
         {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, exprs} <- :erl_parse.parse_exprs(tokens),
         {:value, value, _bindings} <- :erl_eval.exprs(exprs, []) do
      value
    end
  rescue
    error -> {:error, error, __STACKTRACE__}
  end

  defp resolve(term, promise) do
    value = :io_lib.format(~c"~p", [term])
    :emscripten.promise_resolve(promise, value)
  end

  defp split_forms(forms) do
    split_on_dots = fn
      {:dot, _} = f, current -> {:cont, Enum.reverse([f | current]), []}
      f, current -> {:cont, [f | current]}
    end

    ensure_empty_acc = fn [] -> {:cont, []} end

    Enum.chunk_while(forms, [], split_on_dots, ensure_empty_acc)
  end
end
