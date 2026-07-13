defmodule EvalInWasm do
  use GenServer

  @process_name :main

  def start_link(_) do
    GenServer.start_link(__MODULE__, nil, name: @process_name)
  end

  @impl GenServer
  def init(_args), do: {:ok, nil}

  @impl GenServer
  def handle_call([action, code], _from, state) do
    type = as_type(action)

    try do
      {:reply, %{value: inspect(eval(code, type))}, state}
    rescue
      error -> {:reply, %{error: Exception.message(error)}, state}
    end
  end

  defp as_type("eval_elixir"), do: :elixir
  defp as_type("eval_erlang"), do: :erlang
  defp as_type("eval_erlang_module"), do: {:module, :erlang}

  defp eval(code, :elixir) do
    {evaluated, _new_bindings} = Code.eval_string(code, [], __ENV__)
    evaluated
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

    code = :erlang.binary_to_list(code)

    with {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, module, module_bin} <-
           tokens
           |> split_forms()
           |> Enum.map(parse_form)
           |> :compile.noenv_forms(compile_opts),
         {:module, _module} <- :code.load_binary(module, ~c"nofile", module_bin) do
      module
    end
  end

  defp eval(code, :erlang) do
    code = :erlang.binary_to_list(code)

    with {:ok, tokens, _end_location} <- :erl_scan.string(code),
         {:ok, exprs} <- :erl_parse.parse_exprs(tokens),
         {:value, value, _bindings} <- :erl_eval.exprs(exprs, []) do
      value
    end
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
