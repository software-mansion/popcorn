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
      e =
        {{:badmatch, {:error, :enoent}},
         [
           {:ts_config_http, :parse_config, 2,
            [{:file, ~c"src/tsung_controller/ts_config_http.erl"}, {:line, 63}]},
           {:lists, :foldl, 3, [{:file, ~c"lists.erl"}, {:line, 1261}]},
           {:ts_config, :parse, 2,
            [{:file, ~c"src/tsung_controller/ts_config.erl"}, {:line, 437}]},
           {:lists, :foldl, 3, [{:file, ~c"lists.erl"}, {:line, 1261}]},
           {:ts_config, :handle_read, 3,
            [{:file, ~c"src/tsung_controller/ts_config.erl"}, {:line, 85}]},
           {:ts_config, :read, 2, [{:file, ~c"src/tsung_controller/ts_config.erl"}, {:line, 70}]},
           {:ts_config_server, :handle_call, 3,
            [{:file, ~c"src/tsung_controller/ts_config_server.erl"}, {:line, 206}]},
           {:gen_server, :try_handle_call, 4, [{:file, ~c"gen_server.erl"}, {:line, 607}]}
         ]}

      :io.format(~c"~p\n", [e])
      :ok
    end
    |> RunInAtomVM.expr(tmp_dir)
    |> IO.inspect()
  end

  @tag :mod
  test "module", %{tmp_dir: tmp_dir} do
    quote do
      defmodule Adder do
        def dupa(x), do: :ok
        # def blah, do: :blah_ret
      end
    end
    |> eval(tmp_dir)
  end
end
