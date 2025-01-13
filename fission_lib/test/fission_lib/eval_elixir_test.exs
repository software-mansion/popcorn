defmodule FissionLib.EvalElixirTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  setup_all do
    quote do
      if Process.whereis(:elixir_config) == nil, do: :elixir.start([], [])
      # {result, _bindings} = Code.eval_quoted(var!(code))
      {result, _bindings} = Code.eval_string(var!(code), [], __ENV__)
      result
    end
    |> RunInAtomVM.compile("tmp", [:code])

    :ok
  end

  defp eval(code, tmp_dir) do
    code = Macro.to_string(code)
    RunInAtomVM.run("tmp", tmp_dir, code: code)
  end

  test "add", %{tmp_dir: tmp_dir} do
    result =
      quote do
        1 + 2
      end
      |> eval(tmp_dir)

    assert result == 3
  end

  @tag :check
  test "check", %{tmp_dir: tmp_dir} do
    quote do
      :ets.new(:dupa, type: :bag)
      # :ets.info(ets, :type)
      :ok
    end
    |> RunInAtomVM.expr(tmp_dir)
    |> IO.inspect()
  end

  @tag :mod
  test "module", %{tmp_dir: tmp_dir} do
    quote do
      defmodule Adder do
        def dupa, do: :ok
        # def blah, do: :blah_ret
      end
    end
    |> eval(tmp_dir)
  end
end
