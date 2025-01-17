defmodule FissionLib.EvalElixirTest do
  use ExUnit.Case, async: true
  require Logger

  @moduletag :tmp_dir
  setup_all do
    quote do
      if Process.whereis(:elixir_config) == nil, do: :elixir.start([], [])
      # {result, _bindings} = Code.eval_quoted(var!(code))
      # IO.puts("start kernel")
      # :application_controller.start(:kernel)
      # IO.puts("start elixir")
      # :application.ensure_all_started(:elixir)
      # IO.puts("eval code")
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
      # Process.register(self(), :dupa)
      # :erlang.display(:gen.get_proc_name({:local, :dupa}))
      # :erlang.process_info(self(), :registered_name)
      # :erlang.put(:dupa, 1)
      # :erlang.get(:dupa)
      # x = fn a when tuple_size(a) == 2 -> :ok end
      # x.({:a, :b})
      # x = fn a when a != :erlang.node() -> :ok end
      # x.(:a)
      :ets.new(:dupa, [:named_table, :public, {:read_concurrency, true}])
      # :ets.delete(:dupa)
    end
    |> RunInAtomVM.expr(tmp_dir)
    |> IO.inspect()
  end

  # @tag :app
  # test "app", %{tmp_dir: tmp_dir} do
  #   quote do
  #     :application_controller.start(:kernel)
  #     Process.sleep(1000)
  #     :console.print("\n\n-------------------------------------------\n\n")
  #     :application.ensure_all_started(:elixir)
  #     :ok
  #     # pid = Process.whereis(:application_controller)
  #     # :erlang.pid_to_list(pid)
  #   end
  #   |> eval(tmp_dir)
  #   |> IO.inspect()
  # end

  @tag :mod
  test "module", %{tmp_dir: tmp_dir} do
    quote do
      defmodule Adder do
        def dupa(), do: :ok
        # def blah, do: :blah_ret
      end
    end
    |> eval(tmp_dir)
    |> IO.inspect()
  end
end
