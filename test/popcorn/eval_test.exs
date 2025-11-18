defmodule Popcorn.EvalTest do
  use ExUnit.Case, async: true
  require Logger
  require Popcorn.Support.AtomVM
  import AsyncTest
  alias Popcorn.Support.AtomVM

  @examples_path "./test/examples"
  @moduletag :tmp_dir

  async_test "add", %{tmp_dir: dir} do
    "1 + 2."
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(3)
  end

  async_test "case", %{tmp_dir: dir} do
    """
    case lists:max([1,3,2]) of
      3 -> {max, 3};
      2 -> error
    end.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result({:max, 3})
  end

  async_test "send receive", %{tmp_dir: dir} do
    """
    self() ! message,

    receive
      message -> received
    end.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(:received)
  end

  async_test "spawn", %{tmp_dir: dir} do
    """
    Parent = self(),

    Child = spawn(fun() ->
      receive
        ping -> Parent ! pong
      end
    end),

    Child ! ping,

    receive
       pong -> ok
    end,
    ok.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(:ok)
  end

  async_test "closure", %{tmp_dir: dir} do
    """
    A = fun() -> 5 end,
    B = 10,
    A() + B.
    """
    |> AtomVM.eval(:erlang_expr, run_dir: dir)
    |> AtomVM.assert_result(15)
  end

  async_test ":lists.duplicate/2", %{tmp_dir: dir} do
    """
    :lists.duplicate(0, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([])

    """
    :lists.duplicate(1, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(["a"])

    """
    :lists.duplicate(7, "a")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(["a", "a", "a", "a", "a", "a", "a"])
  end

  @tag :skip
  async_test "io_lib", %{tmp_dir: dir} do
    """
    term = {:ok, ["a", 2, 3.0]}
    ~c"~p"
    |> :io_lib.format([term])
    |> to_string()
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("{ok,[<<\"a\">>,2,3.0]}")
  end

  async_test "rescue", %{tmp_dir: dir} do
    """
    try do
      1 + :ok
    rescue
      e -> e
    end
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ArithmeticError{message: "bad argument in arithmetic expression"})
  end

  async_test "Factorial", %{tmp_dir: dir} do
    """
    defmodule Factorial do
      def calc(0), do: 1
      def calc(n) when n > 0, do: n * calc(n - 1)
    end

    Factorial.calc(5)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(120)
  end

  async_test "Fibonacci", %{tmp_dir: dir} do
    """
    defmodule Fibonacci do
        def calc(0) do 0 end
        def calc(1) do 1 end
        def calc(n) do calc(n-1) + calc(n-2) end
    end

    Fibonacci.calc(10)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(55)
  end

  async_test "Adder Elixir", %{tmp_dir: dir} do
    """
    defmodule Adder do
      def calc(a, b), do: a + b
    end

    Adder.calc(10, 20)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(30)
  end

  async_test "Guard", %{tmp_dir: dir} do
    """
    defmodule Guard do
      def f(n) when n > 0, do: f(n-1)
      def f(0), do: :done
    end

    Guard.f(5)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(:done)
  end

  async_test "MultipleFns", %{tmp_dir: dir} do
    """
    defmodule MultipleFns do
      def a(x), do: MultipleFns.b(x) + 1
      def b(x), do: MultipleFns.c(x) + 1
      def c(x), do: MultipleFns.d(x) + 1
      def d(x), do: x
    end

    MultipleFns.a(0)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(3)
  end

  async_test "simple module", %{tmp_dir: dir} do
    """
    defmodule Start do

    end
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Capybara habitat - genserver", %{tmp_dir: dir} do
    "#{@examples_path}/CapybaraHabitat.ex"
    |> File.read!()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Module redefinition", %{tmp_dir: dir} do
    info =
      """
      defmodule W do
        def foo(a, b, c), do: {a, b}
      end

      res1 = W.foo(10, 20, 30)

      defmodule W do
        def bar(a, b, c), do: {b, c}
      end

      res2 = W.bar(10, 20, 30)
      {res1, res2}
      """
      |> AtomVM.try_eval(:elixir, run_dir: dir)

    assert %{exit_status: 0, result: {{10, 20}, {20, 30}}} = info
    # FIXME: Warnings are disabled due to GC problems
    # logs = File.read!(info.log_path)
    # assert String.contains?(logs, "warning: {unused_var,c,false}")
    # assert String.contains?(logs, "warning: {module_defined,'Elixir.RunExpr.W'}")
    # assert String.contains?(logs, "warning: {unused_var,a,false}")
  end

  async_test "Adder Erlang", %{tmp_dir: dir} do
    """
    -module(adder).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "line location", %{tmp_dir: dir} do
    """
    -module(long_function).
    -export([start/0]).

    start() ->
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose",
        User = "Jose".
    """
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "UUID - too big literals", %{tmp_dir: dir} do
    "#{@examples_path}/uuid.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Greetings - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/greetings.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  @tag :skip
  async_test "Capybara habitat - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/capybara_habitat.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "sigils", %{tmp_dir: dir} do
    """
    {
      ~s"a b c",
      ~w(a b c),
      ~c"abc",
      ~s(a #{1}),
      ~D[2019-10-31],
      ~T[23:00:07.0],
      ~N[2019-10-31 23:00:07],
      ~U[2019-10-31 19:59:03Z]
    }
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result({
      "a b c",
      ["a", "b", "c"],
      [?a, ?b, ?c],
      "a 1",
      ~D[2019-10-31],
      ~T[23:00:07.0],
      ~N[2019-10-31 23:00:07],
      ~U[2019-10-31 19:59:03Z]
    })
  end

  @tag :skip
  async_test "non-interpolating string sigil", %{tmp_dir: dir} do
    ~s|~S(a #{1})|
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("a \#{1}")
  end

  async_test "Base module", %{tmp_dir: dir} do
    ~s|Base.encode16("foobar")|
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("666F6F626172")
  end

  async_test "Bitwise module", %{tmp_dir: dir} do
    """
    import Bitwise

    2 ||| 4
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(6)
  end

  async_test "Range module", %{tmp_dir: dir} do
    "Range.disjoint?(1..10, 50..100)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(true)
  end

  async_test "Enum module", %{tmp_dir: dir} do
    "Enum.all?([1,2,3], &(&1 < 5))"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(true)
  end

  async_test "Keyword module", %{tmp_dir: dir} do
    "Keyword.fetch!([a: 1], :a)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(1)
  end

  async_test "Map module", %{tmp_dir: dir} do
    "Map.has_key?(%{a: 1}, :a)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(true)
  end

  async_test "MapSet module", %{tmp_dir: dir} do
    "MapSet.new([:a, :b, :c])"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%MapSet{})
  end

  async_test "List module", %{tmp_dir: dir} do
    "List.duplicate(:ok, 3)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([:ok, :ok, :ok])
  end

  async_test "Tuple module", %{tmp_dir: dir} do
    "Tuple.duplicate(:ok, 3)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result({:ok, :ok, :ok})
  end

  async_test "Integer module", %{tmp_dir: dir} do
    "Integer.to_string(1)"
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("1")
  end

  async_test "ArgumentError", %{tmp_dir: dir} do
    "hd([])"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ArgumentError{})
  end

  async_test "ArithmeticError", %{tmp_dir: dir} do
    ":foo + 1"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ArithmeticError{})
  end

  @tag :skip
  async_test "BadArityError", %{tmp_dir: dir} do
    "Enum.map([1], fn a, b -> :bad end)"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ErlangError{original: :badarity})
  end

  async_test "BadBooleanError", %{tmp_dir: dir} do
    "1 and false"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%BadBooleanError{})
  end

  async_test "BadFunctionError", %{tmp_dir: dir} do
    "Enum.map([1], :not_fun)"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%BadFunctionError{})
  end

  async_test "BadMapError", %{tmp_dir: dir} do
    "Map.get(:not_map, :key)"
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%BadMapError{})
  end

  async_test "BadStructError", %{tmp_dir: dir} do
    """
    defmodule BadStructType do
      def __struct__, do: :invalid
      def __struct__(_), do: :invalid
    end

    struct(BadStructType)
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%ArgumentError{})
  end

  async_test "CaseClauseError", %{tmp_dir: dir} do
    """
    case 1 do
      :n -> :wont_match
    end
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%CaseClauseError{})
  end

  async_test "CondClauseError", %{tmp_dir: dir} do
    """
    cond do
      false -> :wont_match
    end
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%CondClauseError{})
  end

  async_test "FunctionClauseError", %{tmp_dir: dir} do
    """
    f = fn x when is_integer(x) -> :ok end

    f.(:bad)
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%FunctionClauseError{})
  end

  async_test "KeyError", %{tmp_dir: dir} do
    """
    m = %{a: 1}

    m.x
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%KeyError{})
  end

  async_test "SystemLimitError", %{tmp_dir: dir} do
    """
    "x"
    |> List.duplicate(2048)
    |> Enum.join()
    |> String.to_atom()
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%SystemLimitError{})
  end

  async_test "UndefinedFunctionError", %{tmp_dir: dir} do
    """
    Enum.doesnt_exist()
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%UndefinedFunctionError{})
  end

  async_test "WithClauseError", %{tmp_dir: dir} do
    """
    with :not_ok <- :ok do
      :ok
    else
      :not_error -> :ok
    end
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%WithClauseError{})
  end

  async_test "TryClauseError", %{tmp_dir: dir} do
    """
    try do
      :ok
    else
      :not_ok -> :not_ok
    end
    """
    |> wrap_in_try()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(%TryClauseError{})
  end

  defp wrap_in_try(code) do
    """
    try do
      #{code}
    rescue
      e -> e
    end
    """
  end

  async_test ":os.type/0", %{tmp_dir: dir} do
    os_type =
      """
      :os.type()
      """
      |> AtomVM.eval(:elixir, run_dir: dir)

    assert os_type in [{:unix, :darwin}, {:unix, :emscripten}]
  end

  async_test ":filename.split/1", %{tmp_dir: dir} do
    """
    :filename.split("path/to/a/file")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(["path", "to", "a", "file"])
  end

  async_test ":erlang.phash/2 and phash2/2", %{tmp_dir: dir} do
    """
    :erlang.phash({:a, 2.0, "foo"}, 1)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(1)

    """
    :erlang.phash2({:a, 2.0, "foo"}, 1)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(0)

    """
    :erlang.phash({:a, 2.0, "foo"}, 18)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(4)

    """
    :erlang.phash2({:a, 2.0, "foo"}, 18)
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(3)
  end

  async_test "Implement a protocol", %{tmp_dir: dir} do
    quote do
      defimpl Enumerable, for: Tuple do
        def reduce(tuple, acc, fun) do
          Enumerable.reduce(Tuple.to_list(tuple), acc, fun)
        end

        def count(tuple) do
          tuple_size(tuple)
        end

        def member?(tuple, element) do
          Enumerable.member?(Tuple.to_list(tuple), element)
        end

        def slice(tuple) do
          {:ok, tuple_size(tuple), &Tuple.to_list/1}
        end
      end

      Enum.map({1, 2, 3, 4}, fn x -> x + 1 end)
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([2, 3, 4, 5])
  end

  @tag :skip
  @tag timeout: :timer.minutes(5)
  @tag :reraise
  async_test "reraise", %{tmp_dir: dir} do
    {error, stacktrace} =
      quote do
        defmodule Raiser do
          def raise_error() do
            raise "foo"
          end
        end

        defmodule IntermediateCaller do
          def call_raiser() do
            Raiser.raise_error()
            :ok
          end
        end

        defmodule Reraiser do
          def reraise_error() do
            try do
              IntermediateCaller.call_raiser()
            rescue
              e -> reraise e, __STACKTRACE__
            end

            :ok
          end
        end

        try do
          Reraiser.reraise_error()
        rescue
          e -> {e, __STACKTRACE__}
        end
      end
      |> Macro.to_string()
      |> AtomVM.eval(:elixir, run_dir: dir)

    assert error == %RuntimeError{message: "foo"}

    assert [
             {RunExpr.Raiser, :raise_error, 0, _},
             {RunExpr.IntermediateCaller, :call_raiser, 0, _},
             {RunExpr.Reraiser, :reraise_error, 0, _} | _rest
           ] =
             stacktrace
  end

  @tag :logger
  async_test "logger", %{tmp_dir: dir} do
    result =
      quote do
        require Logger
        Logger.debug("foo")
        Logger.info("bar")
        Logger.warning("baz")
        Logger.error("foobar")
      end
      |> Macro.to_string()
      |> AtomVM.try_eval(:elixir, run_dir: dir)

    assert %{exit_status: 0, output: output} = result

    assert [debug, info, warning, error] =
             String.split(output, "\n", trim: true)
             # reject color markers
             |> Enum.reject(&String.starts_with?(&1, "\e"))

    assert debug =~ ~r/\[debug\].*foo/
    assert info =~ ~r/\[info\].*bar/
    assert warning =~ ~r/\[warning\].*baz/
    assert error =~ ~r/\[error\].*foobar/
  end

  @tag :logger
  async_test "GenServer crash handling", %{tmp_dir: dir} do
    result =
      quote do
        defmodule GS do
          use GenServer

          def init(_opts) do
            {:ok, %{}}
          end

          def handle_info(:exit, _state) do
            raise "foo"
          end
        end

        {:ok, pid} = GenServer.start(GS, [])
        Process.monitor(pid)
        send(pid, :exit)

        receive do
          {:DOWN, _ref, :process, ^pid, _reason} -> :ok
        end
      end
      |> Macro.to_string()
      |> AtomVM.try_eval(:elixir, run_dir: dir)

    assert %{result: :ok, exit_status: 0, output: output} = result
    assert output =~ ~r/\[error\].*Generic server .* terminating/
    assert output =~ ~r/"foo"/
  end

  @tag :random
  async_test "random", %{tmp_dir: dir} do
    result =
      quote do
        for _i <- 1..10, do: Enum.random(1..5)
      end
      |> Macro.to_string()
      |> AtomVM.eval(:elixir, run_dir: dir)

    assert length(result) == 10
    Enum.each(result, &assert(&1 in 1..5))
  end

  @tag :format_error
  async_test "Exception.format_error", %{tmp_dir: dir} do
    result =
      quote do
        try do
          # Trigger FunctionClauseError
          String.length(:foo)
        rescue
          e -> Exception.format(:error, e, __STACKTRACE__)
        end
      end
      |> Macro.to_string()
      |> AtomVM.eval(:elixir, run_dir: dir)

    assert "** (FunctionClauseError)" <> _rest = result
  end

  async_test "binary:matches", %{tmp_dir: dir} do
    """
    :binary.matches("abbc", "b")
    """
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([{1, 1}, {2, 1}])
  end

  async_test "module binary literals", %{tmp_dir: dir} do
    quote do
      defmodule MyApp do
        def main(x) do
          "string literal" <> x
        end
      end

      MyApp.main(" another string")
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result("string literal another string")
  end

  async_test "Guards in a module function", %{tmp_dir: dir} do
    quote do
      defmodule NumberChecker do
        def check(x) when is_integer(x) and x > 0 do
          :c1
        end

        def check(x) when x in [1.0, 2.0] do
          :c2
        end

        def check(_x) do
          :c3
        end
      end

      Enum.map([42, 1.0, -1], &NumberChecker.check/1)
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result([:c1, :c2, :c3])
  end

  async_test "Nested modules", %{tmp_dir: dir} do
    quote do
      defmodule Foo do
        defmodule Bar do
          def baz(), do: :ok
        end
      end

      Foo.Bar.baz()
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(:ok)
  end

  async_test "Module attributes", %{tmp_dir: dir} do
    quote do
      defmodule Foo do
        @some_atribute "attribute_value"

        def foo() do
          {@some_atribute, @unknown_attribute}
        end
      end

      Foo.foo()
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result({"attribute_value", nil})
  end

  async_test "Match binary", %{tmp_dir: dir} do
    quote do
      binary = <<1, 2, 3, 4, 5, 6, 7, 8>>

      case binary do
        <<part::binary-size(5), rest::binary>> -> {part, rest}
        _other -> :no_match
      end
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result({<<1, 2, 3, 4, 5>>, <<6, 7, 8>>})
  end

  async_test "Create binary", %{tmp_dir: dir} do
    quote do
      bin1 = <<1, 2, 3, 4>>
      bin2 = <<5, 6, 7, 8>>
      <<bin1::binary-size(2), bin2::binary-size(3)>>
    end
    |> Macro.to_string()
    |> AtomVM.eval(:elixir, run_dir: dir)
    |> AtomVM.assert_result(<<1, 2, 5, 6, 7>>)
  end
end
