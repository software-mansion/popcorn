defmodule FissionLib.HexdocsTestHelper do
  use ExUnit.Case, async: true
  alias FissionLib.Support.AtomVM
  alias __MODULE__, as: Helper
  import FissionLib.Support.AsyncTest

  defmacro test_ast(common, input, opts, tag) do
    quote do
      @tag skip: Keyword.get(unquote(opts), :skip, false)
      @tag unquote(tag)
      async_test Helper.test_name(
                   unquote(common) <> unquote(input),
                   unquote(tag)
                 ),
                 %{tmp_dir: dir} do
        {opts, unsupported} = supported_opts(unquote(opts))
        opts = Map.new(unquote(opts))
        common = unquote(common)
        input = unquote(input)

        if unsupported != [] do
          keys = unsupported |> Keyword.keys() |> inspect()
          IO.warn("Unsupported options: #{keys}")
        end

        info =
          "#{common}\n#{input}"
          |> Helper.maybe_wrap_in_try(opts)
          |> AtomVM.try_eval(:elixir, run_dir: dir)

        assert info.exit_status == 0, """
        Atom VM crashed, check
        \t'#{info.log_path}'
        for logs.

        Snippet:
        #{input}

        Common code:
        #{if common != "", do: common, else: "(empty)"}
        """

        message = fn type, expected ->
          """
          Snippet:
          #{input}

          Failed #{type} assertion
          result: #{inspect(info.result, pretty: true)}
          expected: #{inspect(expected, pretty: true)}

          Common code:
          #{if common != "", do: common, else: "(empty)"}
          """
        end

        case opts do
          %{raises: error} -> assert is_struct(info.result, error), message.(:raises, error)
          %{output: out} -> assert info.result === out, message.(:output, out)
          %{predicate: pred} -> assert pred.(info.result), message.(:predicate, pred)
        end

        case Map.fetch(opts, :stdout) do
          {:ok, stdout} -> assert info.output == stdout
          :error -> :ok
        end
      end
    end
  end

  defmacro create_tests(test_cases, tag: tag) do
    caller = __CALLER__

    test_cases_ast =
      test_cases
      |> Enum.flat_map(fn {input_or_common, opts} ->
        opts
        |> Macro.expand(caller)
        |> Keyword.pop(:cases)
        |> case do
          {nil, opts} ->
            input = input_or_common

            [
              quote do
                test_ast("", unquote(input), unquote(opts), unquote(tag))
              end
            ]

          {cases, opts} ->
            common = input_or_common

            for {case, case_opts} <- cases do
              quote do
                test_ast(
                  unquote(common),
                  unquote(case),
                  Keyword.merge(unquote(opts), unquote(case_opts)),
                  unquote(tag)
                )
              end
            end
        end
      end)

    quote do
      (unquote_splicing(test_cases_ast))
    end
  end

  def as_multiple_cases(common_or_input, opts) do
    case Keyword.fetch(opts, :cases) do
      {:ok, cases} ->
        common = common_or_input

        for case <- cases do
          {common, case}
        end

      :error ->
        input = common_or_input
        [{input, {"", opts}}]
    end
  end

  def maybe_wrap_in_try(code, %{raises: _error}) do
    """
    try do
    #{code}
    rescue
      e -> e
    end
    """
  end

  def maybe_wrap_in_try(code, _opts), do: code

  def supported_opts(opts) do
    supported_opts = [:raises, :output, :predicate, :skip, :cases, :stdout]
    Keyword.split(opts, supported_opts)
  end

  def test_name(input, tag) do
    hash = :crypto.hash(:blake2s, input) |> Base.encode16(case: :lower) |> String.slice(0..8)
    name = String.slice(input, 0..50)

    escaped =
      for <<c <- name>>, into: "" do
        if c in 32..127 and c != ?', do: <<c>>, else: "_"
      end

    "eval_#{to_string(tag)}_#{hash}: #{escaped}"
  end
end

defmodule User do
  defstruct [:name, :age]
end

defmodule FissionLib.HexdocsTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  alias FissionLib.Support.AtomVM
  import FissionLib.HexdocsTestHelper

  @moduletag :tmp_dir

  # Introduction
  [
    {"40 + 2", output: 42},
    {
      """
      "hello" <> " world"
      """,
      output: "hello world"
    }
  ]
  |> create_tests(tag: :introduction)

  # Basic types
  [
    {"40 + 2", output: 42},
    {
      """
      "hello" <> " world"
      """,
      output: "hello world"
    },
    # types
    {"1", predicate: &is_integer/1},
    {"0x1F", predicate: &is_integer/1},
    {"1.0", predicate: &is_float/1},
    {"true", predicate: &is_boolean/1},
    {":atom", predicate: &is_atom/1},
    {~s|"elixir"|, predicate: &String.valid?/1},
    {"[1, 2, 3]\n", predicate: &is_list/1},
    {"{1, 2, 3}\n", predicate: &is_tuple/1},
    # arithmetic
    {"1 + 2", output: 3},
    {"5 * 5", output: 25},
    {"10 / 2", output: 5.0},
    # int division
    {"div(10, 2)", output: 5},
    {"div 10, 2", output: 5},
    {"rem 10, 3", output: 1},
    # bin repressentation
    {"0b1010", output: 10},
    {"0o777", output: 511},
    {"0x1F", output: 31},
    {"1.0", output: 1.0},
    {"1.0e-10", output: 1.0e-10},
    # rounding
    {"round(3.58)", output: 4},
    {"trunc(3.58)", output: 3},
    # is_integer
    {"is_integer(1)", output: true},
    {"is_integer(2.0)", output: false},
    # bools
    {"true", output: true},
    {"true == false", output: false},
    {"true and true", output: true},
    {"false or is_boolean(true)", output: true},
    {"1 and true", raises: BadBooleanError},
    # short circuiting
    {~s|false and raise("This error will never be raised")|, output: false},
    {~s|true or raise("This error will never be raised")|, output: true},
    # non-bool: !, ||, &&
    {"1 || true", output: 1},
    {"false || 11", output: 11},
    {"nil && 13", output: nil},
    {"true && 17", output: 17},
    {"!true", output: false},
    {"!1", output: false},
    {"!nil", output: true},
    # atoms
    {":apple", output: :apple},
    {":orange", output: :orange},
    {":watermelon", output: :watermelon},
    {":apple == :apple", output: true},
    {":apple == :orange", output: false},
    {"true == :true", output: true},
    {"is_atom(false)", output: true},
    {"is_boolean(:false)", output: true},
    # strings, interpolation
    {~s|"hellö"|, output: "hellö"},
    {~s|"hello " <> "world!"|, output: "hello world!"},
    {
      """
      string = "world"
      "hello \#{string}!"
      """,
      output: "hello world!"
    },
    {
      """
      number = 42
      "i am \#{number} years old!"
      """,
      output: "i am 42 years old!"
    },
    {
      """
      "hello
      world"
      """,
      output: "hello\nworld"
    },
    {~s|"hello\nworld"|, output: "hello\nworld"},
    {
      ~s|IO.puts("hello\nworld")|,
      output: :ok, stdout: "hello\nworld", skip: true
    },
    {~s|is_binary("hellö")|, output: true},
    {~s|byte_size("hellö")|, output: 6},
    {~s|String.length("hellö")|, output: 5},
    {~s|String.upcase("hellö")|, output: "HELLÖ"},
    # comparison
    {"1 == 1", output: true},
    {"1 != 2", output: true},
    {"1 < 2", output: true},
    {~s|"foo" == "foo"|, output: true},
    {~s|"foo" == "bar"|, output: false},
    {"1 == 1.0", output: true},
    {"1 == 2.0", output: false},
    {"1 === 1.0", output: false}
  ]
  |> create_tests(tag: :basic_types)

  # Case, cond, and if
  [
    {
      """
      case {1, 2, 3} do
        {1, x, 3} ->
          "This clause will match and bind x to 2 in this clause"
        {4, 5, 6} ->
          "This clause won't match"
        _ ->
          "This clause would match any value"
      end
      """,
      output: "This clause will match and bind x to 2 in this clause"
    },
    {
      """
      x = 1
      case 10 do
        ^x -> "Won't match"
        _ -> "Will match"
      end
      """,
      output: "Will match"
    },
    {
      """
      case {1, 2, 3} do
        {1, x, 3} when x > 0 ->
          "Will match"
        _ ->
          "Would match, if guard condition were not satisfied"
      end
      """,
      output: "Will match"
    },
    {"hd(1)", raises: ArgumentError},
    {
      """
      case 1 do
        x when hd(x) -> "Won't match"
        x -> "Got \#{x}"
      end
      """,
      output: "Got 1"
    },
    {
      """
      case :ok do
        :error -> "Won't match"
      end
      """,
      raises: CaseClauseError
    },
    {
      """
      if true do
        "This works!"
      end
      """,
      output: "This works!"
    },
    {
      """
      if false do
        "This will never be seen"
      end
      """,
      output: nil
    },
    {
      """
      if nil do
        "This won't be seen"
      else
        "This will"
      end
      """,
      output: "This will"
    },
    {
      """
      x = 1
      if true do
        x = x + 1
      end
      """,
      output: 2
    },
    {
      """
      x = 1
      if true do
        x = x + 1
      end
      x
      """,
      output: 1
    },
    {
      """
      x = 1
      x = if true do
        x + 1
      else
        x
      end
      """,
      output: 2
    },
    {
      """
      cond do
        2 + 2 == 5 ->
          "This is never true"
        2 * 2 == 3 ->
          "Nor this"
        true ->
          "This is always true (equivalent to else)"
      end
      """,
      output: "This is always true (equivalent to else)"
    },
    {
      """
      cond do
        hd([1, 2, 3]) ->
          "1 is considered as true"
      end
      """,
      output: "1 is considered as true"
    }
  ]
  |> create_tests(tag: :case_cond_if)

  # Anonymous functions
  [
    {
      """
      add = fn a, b -> a + b end
      add.(1, 2)
      """,
      output: 3
    },
    {
      """
      add = fn a, b -> a + b end
      is_function(add)
      """,
      output: true
    },
    {
      """
      add = fn a, b -> a + b end
      is_function(add, 2)
      """,
      output: true, skip: true
    },
    {
      """
      add = fn a, b -> a + b end
      is_function(add, 1)
      """,
      output: false, skip: true
    },
    {
      """
      add = fn a, b -> a + b end
      double = fn a -> add.(a, a) end
      double.(2)
      """,
      output: 4
    },
    {
      """
      x = 42
      (fn -> x = 0 end).()
      """,
      output: 0
    },
    {
      """
      x = 42
      (fn -> x = 0 end).()
      x
      """,
      output: 42
    },
    {
      """
      f = fn
        x, y when x > 0 -> x + y
        x, y -> x * y
      end
      f.(1, 3)
      """,
      output: 4
    },
    {
      """
      f = fn
        x, y when x > 0 -> x + y
        x, y -> x * y
      end
      f.(-1, 3)
      """,
      output: -3
    },
    {"fun = &is_atom/1", output: &:erlang.is_atom/1},
    {
      """
      fun = &is_atom/1
      is_function(fun)
      """,
      output: true
    },
    {
      """
      fun = &is_atom/1
      fun.(:hello)
      """,
      output: true, skip: true
    },
    {
      """
      fun = &is_atom/1
      fun.(123)
      """,
      output: false, skip: true
    },
    {
      """
      fun = &String.length/1
      """,
      output: &String.length/1
    },
    {
      """
      fun = &String.length/1
      fun.("hello")
      """,
      output: 5
    },
    {
      """
      add = &+/2
      """,
      output: &:erlang.+/2
    },
    {
      """
      add = &+/2
      add.(1, 2)
      """,
      output: 3, skip: true
    },
    {
      """
      add = &+/2
      is_arity_2 = fn fun -> is_function(fun, 2) end
      is_arity_2.(add)
      """,
      output: true, skip: true
    },
    {
      """
      fun = &(&1 + 1)
      fun.(1)
      """,
      output: 2
    },
    {
      """
      fun2 = &"Good \#{&1}"
      fun2.("morning")
      """,
      output: "Good morning"
    }
  ]
  |> create_tests(tag: :anonymous_functions)

  # Binaries
  [
    {
      """
      string = "hello"
      is_binary(string)
      """,
      output: true
    },
    {"?a", output: 97},
    {"?ł", output: 322},
    {~s|"\u0061" == "a"|, output: true},
    {"0x0061 = 97 = ?a", output: 97},
    {
      """
      string = "héllo"
      String.length(string)
      """,
      output: 5
    },
    {
      """
      string = "héllo"
      byte_size(string)
      """,
      output: 6
    },
    {~s|String.codepoints("👩‍🚒")|, output: ["👩", "‍", "🚒"]},
    {~s|String.graphemes("👩‍🚒")|, output: ["👩‍🚒"], skip: true},
    {~s|String.length("👩‍🚒")|, output: 1},
    {~s|"hełło" <> <<0>>|, output: <<104, 101, 197, 130, 197, 130, 111, 0>>},
    {
      ~s|IO.inspect("hełło", binaries: :as_binaries)|,
      output: <<104, 101, 197, 130, 197, 130, 111>>, skip: true
    },
    {"<<42>> == <<42::8>>", output: true},
    {"<<3::4>>", output: <<3::size(4)>>, skip: true},
    {"<<0::1, 0::1, 1::1, 1::1>> == <<3::4>>", output: true, skip: true},
    {"is_bitstring(<<3::4>>)", output: true, skip: true},
    {"is_binary(<<3::4>>)", output: false, skip: true},
    {"is_bitstring(<<0, 255, 42>>)", output: true},
    {"is_binary(<<0, 255, 42>>)", output: true},
    {"is_binary(<<42::16>>)", output: true},
    {
      """
      <<0, 1, x>> = <<0, 1, 2>>
      x
      """,
      output: 2
    },
    {"<<0, 1, x>> = <<0, 1, 2, 3>>", raises: MatchError},
    {
      """
      <<head::binary-size(2), rest::binary>> = <<0, 1, 2, 3>>
      head
      """,
      output: <<0, 1>>, skip: true
    },
    {
      """
      <<head::binary-size(2), rest::binary>> = <<0, 1, 2, 3>>
      rest
      """,
      output: <<2, 3>>, skip: true
    },
    {~s|is_binary("hello")|, output: true},
    {"is_binary(<<239, 191, 19>>)", output: true},
    {"String.valid?(<<239, 191, 19>>)", output: false},
    {~s|"a" <> "ha"|, output: "aha"},
    {"<<0, 1>> <> <<2, 3>>", output: <<0, 1, 2, 3>>},
    {
      """
      <<head, rest::binary>> = "banana"
      head == ?b
      """,
      output: true
    },
    {
      """
      <<head, rest::binary>> = "banana"
      rest
      """,
      output: "anana"
    },
    {~s|"ü" <> <<0>>|, output: <<195, 188, 0>>},
    {
      """
      <<x, rest::binary>> = "über"
      x == ?ü
      """,
      output: false
    },
    {
      """
      <<x, rest::binary>> = "über"
      rest
      """,
      output: <<188, 98, 101, 114>>
    },
    {
      """
      <<x::utf8, rest::binary>> = "über"
      x == ?ü
      """,
      output: true
    },
    {
      """
      <<x::utf8, rest::binary>> = "über"
      rest
      """,
      output: "ber"
    },
    {~s|~c"hello"|, output: ~c"hello"},
    {"[?h, ?e, ?l, ?l, ?o]", output: ~c"hello"},
    {~s|~c"hełło"|, output: [104, 101, 322, 322, 111]},
    {~s|is_list(~c"hełło")|, output: true},
    {
      """
      heartbeats_per_minute = [99, 97, 116]
      inspect(heartbeats_per_minute, charlists: :as_list)
      """,
      output: [99, 97, 116], skip: true
    },
    {~s|to_charlist("hełło")|, output: [104, 101, 322, 322, 111]},
    {~s|to_string(~c"hełło")|, output: "hełło", skip: true},
    {"to_string(1)", output: "1"},
    {~s|~c"this " <> ~c"fails"|, raises: ArgumentError, skip: true},
    {~s|~c"this " ++ ~c"works"|, output: ~c"this works"},
    {~s|"he" ++ "llo"|, raises: ArgumentError},
    {~s|"he" <> "llo"|, output: "hello"}
  ]
  |> create_tests(tag: :binaries)

  # Keyword lists, maps
  [
    {~s|String.split("1 2 3 4", " ")|, output: ["1", "2", "3", "4"]},
    {~s|String.split("1 2 3 4", " ", [parts: 3])|, output: ["1", "2", "3 4"]},
    {~s|String.split("1  2  3  4", " ", [parts: 3])|, output: ["1", "", "2  3  4"]},
    {~s|String.split("1  2  3  4", " ", [parts: 3, trim: true])|,
     output: ["1", "2", " 3  4"]},
    {"[{:parts, 3}, {:trim, true}] == [parts: 3, trim: true]", output: true},
    {
      """
      import String, only: [split: 1, split: 2]
      split("hello world")
      """,
      output: ["hello", "world"]
    },
    {
      "list = [a: 1, b: 2]",
      cases: [
        {"list ++ [c: 3]", output: [a: 1, b: 2, c: 3]},
        {"[a: 0] ++ list", output: [a: 0, a: 1, b: 2]},
        {"list[:a]", output: 1},
        {"list[:b]", output: 2}
      ]
    },
    {
      """
      [a: a] = [a: 1]
      a
      """,
      output: 1
    },
    {"[a: a] = [a: 1, b: 2]", raises: MatchError},
    {"[b: b, a: a] = [a: 1, b: 2]", raises: MatchError},
    {
      "map = %{:a => 1, 2 => :b}",
      cases: [
        {"map[:a]", output: 1},
        {"map[2]", output: :b},
        {"map[:c]", output: nil}
      ]
    },
    {
      "map = %{:a => 1, 2 => :b}",
      cases: [
        {"%{} = map", output: %{:a => 1, 2 => :b}},
        {
          """
          %{:a => a} = map
          a
          """,
          output: 1
        },
        {"%{:c => c} = map", raises: MatchError},
        {"Map.get(map, :a)", output: 1},
        {"Map.put(map, :c, 3)", output: %{2 => :b, :a => 1, :c => 3}},
        {"Map.to_list(map)", output: [{2, :b}, {:a, 1}]}
      ]
    },
    {
      ~s|map = %{:name => "John", :age => 23}|,
      cases: [
        {"map.name", output: "John"},
        {"map.agee", raises: KeyError},
        {~s/%{map | name: "Mary"}/, output: %{name: "Mary", age: 23}},
        {"%{map | agee: 27}", raises: KeyError}
      ]
    },
    {
      """
      users = [
        john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
        mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
      ]
      """,
      cases: [
        {"users[:john].age", output: 27},
        {
          "put_in(users[:john].age, 31)",
          output: [
            john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
            mary: %{age: 29, languages: ["Elixir", "F#", "Clojure"], name: "Mary"}
          ]
        },
        {
          ~s|update_in(users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end)|,
          output: [
            john: %{age: 27, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
            mary: %{age: 29, languages: ["Elixir", "F#"], name: "Mary"}
          ]
        }
      ]
    }
  ]
  |> create_tests(tag: :kw_maps)

  # Modules
  [
    {~s|String.length("hello")|, output: 5},
    {
      """
      defmodule Math do
        def sum(a, b) do
          a + b
        end
      end

      Math.sum(1, 2)
      """,
      output: 3
    },
    {
      """
      defmodule Math do
        def sum(a, b) do
          do_sum(a, b)
        end

        defp do_sum(a, b) do
          a + b
        end
      end
      Math.do_sum(1,2)
      """,
      raises: UndefinedFunctionError
    },
    {
      """
      defmodule Math do
        def zero?(0) do
          true
        end

        def zero?(x) when is_integer(x) do
          false
        end
      end
      """,
      cases: [
        {"", predicate: &AtomVM.assert_is_module/1},
        {"Math.zero?(0)", output: true},
        {"Math.zero?(1)", output: false},
        {"Math.zero?([1, 2, 3])", raises: FunctionClauseError},
        {"Math.zero?(0.0)", raises: FunctionClauseError, skip: true}
      ]
    },
    {
      """
      defmodule Concat do
        def join(a, b, sep \\\\ " ") do
          a <> sep <> b
        end
      end
      """,
      skip: true,
      cases: [
        {~s|Concat.join("Hello", "world")|, output: "Hello world"},
        {~s|Concat.join("Hello", "world", "_")|, output: "Hello_world"}
      ]
    },
    {
      """
      defmodule Concat do
        # A function head declaring defaults
        def join(a, b, sep \\\\ " ")

        def join(a, b, _sep) when b == "" do
          a
        end

        def join(a, b, sep) do
          a <> sep <> b
        end
      end
      """,
      skip: true,
      cases: [
        {~s|Concat.join("Hello", "")|, output: "Hello"},
        {~s|Concat.join("Hello", "world", " ")|, output: "Hello world"},
        {~s|Concat.join("Hello", "world", "_")|, output: "Hello_world"}
      ]
    },
    {
      """
      defmodule DefaultTest do
        def dowork(x \\\\ "hello") do
          x
        end
      end
      """,
      skip: true,
      cases: [
        {"DefaultTest.dowork()", output: "hello"},
        {"DefaultTest.dowork(123)", output: 123}
      ]
    }
  ]
  |> create_tests(tag: :modules)

  # Recursion
  [
    {
      """
      defmodule Recursion do
        def print_multiple_times(msg, n) when n > 0 do
          IO.puts(msg)
          print_multiple_times(msg, n - 1)
        end

        def print_multiple_times(_msg, 0) do
          :ok
        end
      end
      """,
      cases: [
        {
          ~s|Recursion.print_multiple_times("Hello!", 3)|,
          output: :ok, stdout: "Hello!\nHello!\nHello!\n"
        },
        {
          ~s|Recursion.print_multiple_times("Hello!", -1)|,
          raises: FunctionClauseError
        }
      ]
    },
    {
      """
      defmodule Math do
        def sum_list([head | tail], accumulator) do
          sum_list(tail, head + accumulator)
        end

        def sum_list([], accumulator) do
          accumulator
        end
      end
      """,
      cases: [
        {"Math.sum_list([1, 2, 3], 0)", output: 6},
        {"Math.sum_list([2, 3], 1)", output: 6},
        {"Math.sum_list([3], 3)", output: 6},
        {"Math.sum_list([], 6)", output: 6}
      ]
    },
    {
      """
      defmodule Math do
        def double_each([head | tail]) do
          [head * 2 | double_each(tail)]
        end

        def double_each([]) do
          []
        end
      end

      Math.double_each([1, 2, 3])
      """,
      output: [2, 4, 6]
    },
    {"Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)", output: 6},
    {"Enum.map([1, 2, 3], fn x -> x * 2 end)", output: [2, 4, 6]},
    {"Enum.map([1, 2, 3], &(&1 * 2))", output: [2, 4, 6]}
  ]
  |> create_tests(tag: :recursion)

  # Enumerables
  [
    {"Enum.map(%{1 => 2, 3 => 4}, fn {k, v} -> k * v end)", output: [2, 12]},
    {"Enum.map(1..3, fn x -> x * 2 end)", output: [2, 4, 6]},
    {
      """
      Enum.reduce(1..3, 0, &+/2)
      """,
      output: 6, skip: true
    },
    {
      """
      odd? = fn x -> rem(x, 2) != 0 end
      Enum.filter(1..3, odd?)
      """,
      output: [1, 3]
    },
    {
      """
      odd? = fn x -> rem(x, 2) != 0 end
      1..100_000 |> Enum.map(&(&1 * 3)) |> Enum.filter(odd?) |> Enum.sum()
      """,
      # slow
      output: 7_500_000_000, skip: true
    },
    {
      """
      odd? = fn x -> rem(x, 2) != 0 end
      Enum.sum(Enum.filter(Enum.map(1..100_000, &(&1 * 3)), odd?))
      """,
      # slow
      output: 7_500_000_000, skip: true
    },
    {
      """
      odd? = fn x -> rem(x, 2) != 0 end
      1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?) |> Enum.sum()
      """,
      # slow
      output: 7_500_000_000, skip: true
    },
    {"1..100_000 |> Stream.map(&(&1 * 3))", predicate: &is_struct(&1, Stream), skip: true},
    {
      """
      stream = Stream.cycle([1, 2, 3])
      Enum.take(stream, 10)
      """,
      output: [1, 2, 3, 1, 2, 3, 1, 2, 3, 1]
    }
  ]
  |> create_tests(tag: :enums)

  # Processes
  [
    {"spawn(fn -> 1 + 2 end)", predicate: &is_pid/1, skip: true},
    {
      """
      pid = spawn(fn -> 1 + 2 end)
      Process.sleep(50) # 50ms
      Process.alive?(pid)
      """,
      output: false
    },
    {"Process.alive?(self())", output: true},
    {
      """
      send(self(), {:hello, "world"})
      receive do
        {:hello, msg} -> msg
        {:world, _msg} -> "won't match"
      end
      """,
      output: "world"
    },
    {
      """
      receive do
        {:hello, msg}  -> msg
      after
        100 -> "nothing after 100ms"
      end
      """,
      output: "nothing after 100ms"
    },
    {
      """
      parent = self()
      spawn(fn -> send(parent, {:hello, self()}) end)
      receive do
        {:hello, pid} -> "Got hello from \#{inspect(pid)}"
      end
      """,
      predicate: &(&1 =~ ~r/Got hello from #PID<0\.\d+\.0>/)
    },
    {~s|spawn(fn -> raise "oops" end)|, raises: RuntimeError, skip: true},
    {~s|spawn_link(fn -> raise "oops" end)|, raises: RuntimeError, skip: true},
    {~s|Task.start(fn -> raise "oops" end)|, raises: RuntimeError, skip: true},
    {
      """
      defmodule KV do
        def start_link do
          Task.start_link(fn -> loop(%{}) end)
        end

        defp loop(map) do
          receive do
            {:get, key, caller} ->
              send(caller, Map.get(map, key))
              loop(map)
            {:put, key, value} ->
              loop(Map.put(map, key, value))
          end
        end
      end
      {:ok, pid} = KV.start_link()
      """,
      cases: [
        {
          """
          send(pid, {:get, :hello, self()})
          receive do
            msg -> msg
          end
          """,
          output: nil
        },
        {
          """
          send(pid, {:put, :hello, :world})
          send(pid, {:get, :hello, self()})
          receive do
            msg -> msg
          end
          """,
          output: :world
        },
        {
          """
          Process.register(pid, :kv)
          send(:kv, {:put, :hello, :world})
          send(:kv, {:get, :hello, self()})
          receive do
            msg -> msg
          end
          """,
          output: :world
        }
      ],
      skip: true
    },
    {
      """
      {:ok, pid} = Agent.start_link(fn -> %{} end)
      Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
      Agent.get(pid, fn map -> Map.get(map, :hello) end)
      """,
      output: :world
    }
  ]
  |> create_tests(tag: :processes)

  # IO
  # Most are commented out: we're not supporting stdin and we don't have sample file
  [
    {~s|IO.puts("hello world")|, output: :ok, stdout: "hello world\n"},
    # {~s|IO.gets("yes or no? ")|, stdin: "yes\n"},
    # {
    #   """
    #   IO.puts(:stderr, "hello world")
    #   """,
    #   output: :ok, stderr: "hello world\n"
    # },
    # {
    #   """
    #   {:ok, file} = File.open("path/to/file/hello", [:write])
    #   IO.binwrite(file, "world")
    #   File.close(file)
    #   File.read("path/to/file/hello")
    #   """, output: "world"
    # },
    # {~s|File.read("path/to/file/hello")|, output: {:ok, "world"}},
    # {~s|File.read!("path/to/file/hello")|, output: "world"},
    # {~s|File.read!("path/to/file/unknown")|, output: {:error, :enoent}},
    # {~s|File.read!("path/to/file/unknown")|, raises: File.Error},
    # {~s|Path.expand("~/hello")|, output: "/Users/jose/hello"},
    # {
    #   """
    #   {:ok, file} = File.open("hello")
    #   :ok = File.close(file)
    #   IO.write(file, "is anybody out there")
    #   """,
    #   raises: ErlangError
    # },
    # {
    #   """
    #   pid = spawn(fn ->
    #     receive do
    #       msg -> IO.inspect(msg)
    #     end
    #   end)
    #   IO.write(pid, "hello")
    #   """,
    #   predicate: &match?({:io_request, _pid, _ref, {:put_chars, :unicode, "hello"}}, &1)
    # },
    {
      """
      name = "Mary"
      IO.puts("Hello " <> name <> "!")
      """,
      output: :ok, stdout: "Hello Mary!\n"
    },
    {
      """
      name = "Mary"
      IO.puts(["Hello ", name, "!"])
      """,
      output: :ok, stdout: "Hello Mary!\n"
    },
    {
      ~s|Enum.join(["apple", "banana", "lemon"], ",")|,
      output: "apple,banana,lemon"
    },
    {
      ~s|Enum.intersperse(["apple", "banana", "lemon"], ",")|,
      output: ["apple", ",", "banana", ",", "lemon"]
    },
    {
      ~s|IO.puts(["apple", [",", "banana", [",", "lemon"]]])|,
      output: :ok, stdout: "apple,banana,lemon\n"
    },
    {
      ~s|IO.puts(["apple", ?,, "banana", ?,, "lemon"])|,
      output: :ok, stdout: "apple,banana,lemon\n"
    },
    {
      ~s|IO.puts([?O, ?l, ?á, ?\\s, "Mary", ?!])|,
      output: :ok, stdout: "Olá Mary!\n", skip: true
    }
  ]
  |> create_tests(tag: :io)

  # Alias, require, use
  [
    {
      """
      defmodule Math do
        defmodule List do
        end
      end
      """,
      cases: [
        {
          """
          alias Math.List
          List
          """,
          output: Math.List
        },
        {
          """
          alias Math.List, as: List
          List
          """,
          output: Math.List
        }
      ],
      skip: true
    },
    {"Integer.is_odd(3)", raises: UndefinedFunctionError},
    {
      """
      require Integer
      Integer.is_odd(3)
      """,
      output: true
    },
    {
      """
      import List, only: [duplicate: 2]
      duplicate(:ok, 3)
      """,
      output: [:ok, :ok, :ok]
    },
    {
      """
      defmodule Math do
        def some_function do
          import List, only: [duplicate: 2]
          duplicate(:ok, 10)
        end
      end

      Math.some_function()
      """,
      output: [:ok, :ok, :ok, :ok, :ok, :ok, :ok, :ok, :ok, :ok]
    },
    {"is_atom(String)", output: true},
    {"to_string(String)", output: "Elixir.String"},
    {"to_string(String)", output: "Elixir.String"},
    {~s|:"Elixir.String" == String|, output: true},
    {"List.flatten([1, [2], 3])", output: [1, 2, 3]},
    {~s|:"Elixir.List".flatten([1, [2], 3])|, output: [1, 2, 3]},
    {":lists.flatten([1, [2], 3])", output: [1, 2, 3]},
    {
      """
      defmodule Foo do
        defmodule Bar do
        end
      end
      """,
      predicate: &AtomVM.assert_is_module/1, skip: true
    },
    {
      """
      defmodule Foo.Bar do
      end

      defmodule Foo do
        alias Foo.Bar
        # Can still access it as `Bar`
      end
      """,
      predicate: &AtomVM.assert_is_module/1, skip: true
    },
    {
      """
      defmodule Foo do
        defmodule Bar do
          defmodule Baz do
          end
        end
      end

      alias Foo.Bar.Baz
      # The module `Foo.Bar.Baz` is now available as `Baz`
      # However, the module `Foo.Bar` is *not* available as `Bar`
      """,
      output: Foo.Bar.Baz, skip: true
    }
  ]
  |> create_tests(tag: :alias_require)

  # Module attributes
  [
    {
      """
      defmodule MyServer do
        @moduledoc "My server code."
      end
      """,
      predicate: &AtomVM.assert_is_module/1
    },
    {
      """
      defmodule Math do
        @moduledoc \"""
        Provides math-related functions.

        ## Examples

            Math.sum(1, 2)
            3

        \"""

        @doc \"""
        Calculates the sum of two numbers.
        \"""
        def sum(a, b), do: a + b
      end
      """,
      predicate: &AtomVM.assert_is_module/1
    },
    {
      """
      defmodule MyServer do
        @service URI.parse("https://example.com")
        IO.inspect(@service)
      end
      """,
      predicate: &AtomVM.assert_is_module/1, skip: true
    },
    {
      """
      defmodule MyServer do
        @unknown
      end
      """,
      predicate: &AtomVM.assert_is_module/1
    }
  ]
  |> create_tests(tag: :module_attrs)

  # Structs
  [
    {
      """
      map = %{a: 1, b: 2}
      map[:a]
      """,
      output: 1
    },
    {
      """
      map = %{a: 1, b: 2}
      %{map | a: 3}
      """,
      output: %{a: 3, b: 2}
    },
    {
      """
      defmodule User do
        defstruct name: "John", age: 27
      end
      """,
      cases: [
        {"", predicate: &AtomVM.assert_is_module/1},
        {"%User{}", output: %User{age: 27, name: "John"}},
        {~s|%User{name: "Jane"}|, output: %User{age: 27, name: "Jane"}},
        {"%User{oops: :field}", raises: KeyError},
        {
          """
          john = %User{}
          jane = %{john | name: "Jane"}
          %{jane | oops: :field}
          """,
          raises: KeyError
        },
        {"%User{} = %{}", raises: MatchError},
        {
          """
          john = %User{}
          {is_map(john), john.__struct__}
          """,
          output: {true, User}
        },
        {
          """
          john = %User{}
          john[:name]
          """,
          raises: UndefinedFunctionError
        },
        {
          """
          john = %User{}
          Enum.each(john, fn {field, value} -> IO.puts(value) end)
          """,
          raises: Protocol.UndefinedError
        }
      ],
      skip: true
    },
    {
      """
      defmodule Car do
        @enforce_keys [:make]
        defstruct [:model, :make]
      end
      %Car{}
      """,
      raises: ArgumentError, skip: true
    }
  ]
  |> create_tests(tag: :structs)

  # Protocols
  [
    {
      """
      defprotocol Utility do
        @spec type(t) :: String.t()
        def type(value)
      end

      defimpl Utility, for: BitString do
        def type(_value), do: "string"
      end

      defimpl Utility, for: Integer do
        def type(_value), do: "integer"
      end

      Utility.type("foo")
      Utility.type(123)
      """,
      output: "integer", skip: true
    },
    {
      """
      defprotocol Size do
        @doc "Calculates the size (and not the length!) of a data structure"
        def size(data)
      end

      defimpl Size, for: BitString do
        def size(string), do: byte_size(string)
      end

      defimpl Size, for: Map do
        def size(map), do: map_size(map)
      end

      defimpl Size, for: Tuple do
        def size(tuple), do: tuple_size(tuple)
      end
      """,
      cases: [
        {~s|Size.size("foo")|, output: 3},
        {~s|Size.size({:ok, "hello"})|, output: 2},
        {~s|Size.size(%{label: "some label"})|, output: 1},
        {"Size.size([1, 2, 3])", raises: Protocol.UndefinedError},
        {"Size.size(%{})", output: 0},
        {
          """
          set = %MapSet{} = MapSet.new([])
          Size.size(set)
          """,
          raises: Protocol.UndefinedError
        },
        {
          """
          defimpl Size, for: MapSet do
            def size(set), do: MapSet.size(set)
          end

          Size.size(set)
          """,
          output: 0
        }
      ],
      skip: true
    },
    {"Enum.reduce(1..3, 0, fn x, acc -> x + acc end)", output: 6},
    {~s|"age: \#{25}"|, output: "age: 25"},
    {
      """
      tuple = {1, 2, 3}
      "tuple: \#{tuple}"
      """,
      output: Protocol.UndefinedError, skip: true
    },
    {
      """
      tuple = {1, 2, 3}
      "tuple: \#{inspect(tuple)}"
      """,
      output: "tuple: {1, 2, 3}"
    },
    {"inspect &(&1+2)", predicate: &(&1 =~ "#Function"), skip: true}
  ]
  |> create_tests(tag: :protocols)

  # Comprehensions
  [
    {"for n <- [1, 2, 3, 4], do: n * n", output: [1, 4, 9, 16]},
    {"for n <- 1..4, do: n * n", output: [1, 4, 9, 16]},
    {
      """
      values = [good: 1, good: 2, bad: 3, good: 4]
      for {:good, n} <- values, do: n * n
      """,
      output: [1, 4, 16]
    },
    {"for n <- 0..5, rem(n, 3) == 0, do: n * n", output: [0, 9]},
    # {
    #   """
    #   dirs = ["/home/mikey", "/home/james"]

    #   for dir <- dirs,
    #       file <- File.ls!(dir),
    #       path = Path.join(dir, file),
    #       File.regular?(path) do
    #     File.stat!(path).size
    #   end
    #   """,
    #   output: :todo
    # },
    {
      "for i <- [:a, :b, :c], j <- [1, 2], do: {i, j}",
      output: [a: 1, a: 2, b: 1, b: 2, c: 1, c: 2]
    },
    {
      """
      pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
      for <<r::8, g::8, b::8 <- pixels>>, do: {r, g, b}
      """,
      output: [{213, 45, 132}, {64, 76, 32}, {76, 0, 0}, {234, 32, 15}]
    },
    {~s|for <<c <- " hello world ">>, c != ?\\s, into: "", do: <<c>>|, output: "helloworld"},
    {
      ~s|for {key, val} <- %{"a" => 1, "b" => 2}, into: %{}, do: {key, val * val}|,
      output: %{"a" => 1, "b" => 4}
    }
    # {
    #   """
    #   stream = IO.stream(:stdio, :line)
    #   for line <- stream, into: stream do
    #     String.upcase(line) <> "\n"
    #   end
    #   """,
    #   output: :todo
    # }
  ]
  |> create_tests(tag: :comprehensions)

  # Sigils
  [
    {
      """
      # A regular expression that matches strings which contain "foo" or "bar":
      regex = ~r/foo|bar/
      "foo" =~ ~r/foo|bar/
      """,
      output: true, skip: true
    },
    {
      """
      # A regular expression that matches strings which contain "foo" or "bar":
      regex = ~r/foo|bar/
      "bat" =~ regex
      """,
      output: true, skip: true
    },
    {~s|"HELLO" =~ ~r/hello/|, output: false, skip: true},
    {~s|~r/hello/|, output: ~r/hello/, skip: true},
    {"~w(foo bar bat)a", output: [:foo, :bar, :bat], skip: true},
    {
      ~s|~s(String with escape codes \x26 \#{"inter" <> "polation"})|,
      output: "String with escape codes & interpolation"
    },
    {
      "~S(String without escape codes \\x26 without \#{interpolation})",
      output: "String without escape codes \\x26 without \#{interpolation}"
    },
    {
      """
      d = ~D[2019-10-31]
      d.day
      """,
      output: 31
    },
    {
      """
      t = ~T[23:00:07.0]
      t.second
      """,
      output: 7
    },
    {"~N[2019-10-31 23:00:07]", output: ~N[2019-10-31 23:00:07]},
    {
      """
      dt = ~U[2019-10-31 19:59:03Z]
      %DateTime{minute: minute, time_zone: time_zone} = dt
      minute
      """,
      output: 59
    },
    {
      """
      dt = ~U[2019-10-31 19:59:03Z]
      %DateTime{minute: minute, time_zone: time_zone} = dt
      time_zone
      """,
      output: "Etc/UTC"
    },
    {~s|sigil_r(<<"foo">>, [?i])|, output: ~r"foo"i, skip: true},
    {
      """
      defmodule MySigils do
        def sigil_i(string, []), do: String.to_integer(string)
        def sigil_i(string, [?n]), do: -String.to_integer(string)
      end
      import MySigils
      """,
      cases: [
        {"~i(13)", output: 13},
        {"~i(42)n", output: -42}
      ],
      skip: true
    }
  ]
  |> create_tests(tag: :sigils)

  # Try, catch, raise
  [
    {":foo + 1", raises: ArithmeticError},
    {~s|raise "oops"|, raises: RuntimeError},
    {~s|raise ArgumentError, message: "invalid argument foo"|, raises: ArgumentError},
    {
      """
      defmodule MyError do
        defexception message: "default message"
      end
      raise MyError
      """,
      raises: MyError, skip: true
    },
    {
      """
      try do
        raise "oops"
      rescue
        e in RuntimeError -> e
      end
      """,
      output: %RuntimeError{message: "oops"}
    },
    {
      """
      try do
        raise "oops"
      rescue
        RuntimeError -> "Error!"
      end
      """,
      output: "Error!"
    },
    {
      """
      try do
        Enum.each(-50..50, fn x ->
          if rem(x, 13) == 0, do: throw(x)
        end)
        "Got nothing"
      catch
        x -> "Got \#{x}"
      end
      """,
      output: "Got -39"
    },
    {"Enum.find(-50..50, &(rem(&1, 13) == 0))", output: -39},
    {"spawn_link(fn -> exit(1) end)", raises: :EXIT, skip: true},
    {
      """
      try do
        exit("I am exiting")
      catch
        :exit, _ -> "not really"
      end
      """,
      output: "not really"
    },
    {
      """
      x = 2
      try do
        1 / x
      rescue
        ArithmeticError ->
          :infinity
      else
        y when y < 1 and y > -1 ->
          :small
        _ ->
          :large
      end
      """,
      output: :small
    },
    {
      """
      what_happened =
        try do
          raise "fail"
          :did_not_raise
        rescue
          _ -> :rescued
        end
      what_happened
      """,
      output: :rescued
    }
  ]
  |> create_tests(tag: :try_catch)

  # Optional syntax
  [
    {"length([1, 2, 3]) == length [1, 2, 3]", output: true},
    {
      """
      # do-end blocks
      if true do
        :this
      else
        :that
      end
      """,
      output: :this
    },
    {"if true, do: :this, else: :that", output: :this},
    {
      """
      defmodule(Math, [
        {:do, def(add(a, b), [{:do, a + b}])}
      ])
      """,
      predicate: &AtomVM.assert_is_module/1
    }
  ]
  |> create_tests(tag: :optional_syntax)

  # Erlang libraries
  [
    {"is_atom(String)", output: true},
    {~s|String.first("hello")|, output: "h"},
    {"is_atom(:binary)", output: true},
    {~s|:binary.first("hello")|, output: 104},
    {~s|String.to_charlist("Ø")|, output: [216]},
    {~s|:binary.bin_to_list("Ø")|, output: [195, 152]},
    {~s|:io.format("Pi is approximately given by:~10.3f~n", [:math.pi])|,
     output: :ok, skip: true},
    {
      ~s|to_string(:io_lib.format("Pi is approximately given by:~10.3f~n", [:math.pi]))|,
      output: "Pi is approximately given by:     3.142\n"
    },
    {
      ~s|Base.encode16(:crypto.hash(:sha256, "Elixir"))|,
      output: "3315715A7A3AD57428298676C5AE465DADA38D951BDFAC9348A8A31E9C7401CB"
    },
    {
      """
      digraph = :digraph.new()
      coords = [{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}]
      [v0, v1, v2] = (for c <- coords, do: :digraph.add_vertex(digraph, c))
      :digraph.add_edge(digraph, v0, v1)
      :digraph.add_edge(digraph, v1, v2)
      :digraph.get_short_path(digraph, v0, v2)
      """,
      output: [{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}], skip: true
    },
    {
      """
      table = :ets.new(:ets_test, [])
      # Store as tuples with {name, population}
      :ets.insert(table, {"China", 1_374_000_000})
      :ets.insert(table, {"India", 1_284_000_000})
      :ets.insert(table, {"USA", 322_000_000})
      :ets.tab2list(table)
      """,
      output: :todo, skip: true
    },
    {
      """
      angle_45_deg = :math.pi() * 45.0 / 180.0
      :math.sin(angle_45_deg)
      """,
      output: 0.7071067811865475
    },
    {":math.exp(55.0)", output: 7.694785265142018e23},
    {":math.log(7.694785265142018e23)", output: 55.0},
    {
      """
      q = :queue.new
      q = :queue.in("A", q)
      q = :queue.in("B", q)
      """,
      cases: [
        {
          """
          {value, q} = :queue.out(q)
          value
          """,
          output: {:value, "A"}
        },
        {
          """
          {value, q} = :queue.out(q)
          {value, q} = :queue.out(q)
          value
          """,
          output: {:value, "B"}
        },
        {
          """
          {value, q} = :queue.out(q)
          {value, q} = :queue.out(q)
          {value, q} = :queue.out(q)
          value
          """,
          output: :empty
        }
      ]
    },
    {":rand.uniform()", predicate: &is_float/1, skip: true},
    {
      """
      _ = :rand.seed(:exs1024, {123, 123534, 345345})
      :rand.uniform()
      """,
      output: :todo, skip: true
    },
    {":rand.uniform(6)", predicate: &is_integer/1, skip: true},
    {
      """
      song = "
      Mary had a little lamb,
      His fleece was white as snow,
      And everywhere that Mary went,
      The lamb was sure to go."
      compressed = :zlib.compress(song)
      """,
      cases: [
        {"byte_size(song)", output: 110},
        {"byte_size(compressed)", output: 99}
      ]
    },
    {
      """
      song = "
      Mary had a little lamb,
      His fleece was white as snow,
      And everywhere that Mary went,
      The lamb was sure to go."
      compressed = :zlib.compress(song)
      :zlib.uncompress(compressed)
      """,
      output:
        "\nMary had a little lamb,\nHis fleece was white as snow,\nAnd everywhere that Mary went,\nThe lamb was sure to go.",
      skip: true
    }
  ]
  |> create_tests(tag: :erlang_libraries)

  # Debugging
  [
    {
      """
      (1..10)
      |> IO.inspect()
      |> Enum.map(fn x -> x * 2 end)
      |> IO.inspect()
      |> Enum.sum()
      |> IO.inspect()
      """,
      output: 110,
      stdout: """
      1..10
      [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
      110
      """
    },
    {
      """
      [1, 2, 3]
      |> IO.inspect(label: "before")
      |> Enum.map(&(&1 * 2))
      |> IO.inspect(label: "after")
      |> Enum.sum
      """,
      output: 12,
      stdout: """
      before: [1, 2, 3]
      after: [2, 4, 6]
      """,
      skip: true
    }
  ]
  |> create_tests(tag: :debugging)

  # Enum cheatsheet
  [
    {
      """
      cart = [
        %{fruit: "apple", count: 3},
        %{fruit: "banana", count: 1},
        %{fruit: "orange", count: 6}
      ]
      """,
      cases: [
        {~s|Enum.any?(cart, & &1.fruit == "orange")|, output: true},
        {~s|Enum.any?(cart, & &1.fruit == "pear")|, output: false},
        {"Enum.all?(cart, & &1.count > 0)", output: true},
        {"Enum.all?(cart, & &1.count > 1)", output: false},
        {~s|Enum.member?(cart, %{fruit: "apple", count: 3})|, output: true},
        {"Enum.member?(cart, :something_else)", output: false},
        {~s|%{fruit: "apple", count: 3} in cart|, output: true},
        {":something_else in cart", output: false},
        {"Enum.empty?(cart)", output: false},
        {~s|Enum.filter(cart, &(&1.fruit =~ "o"))|,
         output: [%{fruit: "orange", count: 6}], skip: true},
        {
          ~s|Enum.filter(cart, &(&1.fruit =~ "e"))|,
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6}
          ],
          skip: true
        },
        {
          ~s|Enum.reject(cart, &(&1.fruit =~ "o"))|,
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1}
          ],
          skip: true
        },
        {
          """
          Enum.flat_map(cart, fn item ->
            if item.count > 1, do: [item.fruit], else: []
          end)
          """,
          output: ["apple", "orange"]
        },
        {
          """
          for item <- cart, item.fruit =~ "e" do
            item
          end
          """,
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6}
          ],
          skip: true
        },
        {
          """
          for %{count: 1, fruit: fruit} <- cart do
            fruit
          end
          """,
          output: ["banana"]
        },
        {"Enum.map(cart, & &1.fruit)", output: ["apple", "banana", "orange"]},
        {
          """
          Enum.map(cart, fn item ->
            %{item | count: item.count + 10}
          end)
          """,
          output: [
            %{fruit: "apple", count: 13},
            %{fruit: "banana", count: 11},
            %{fruit: "orange", count: 16}
          ]
        },
        {
          """
          Enum.map_every(cart, 2, fn item ->
            %{item | count: item.count + 10}
          end)
          """,
          output: [
            %{fruit: "apple", count: 13},
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 16}
          ]
        },
        {
          """
          for item <- cart do
            item.fruit
          end
          """,
          output: ["apple", "banana", "orange"]
        },
        {
          """
          for item <- cart, item.fruit =~ "e" do
            item.fruit
          end
          """,
          output: ["apple", "orange"], skip: true
        },
        {
          "Enum.each(cart, &IO.puts(&1.fruit))",
          output: :ok,
          stdout: """
          apple
          banana
          orange
          """
        },
        {
          """
          Enum.reduce(cart, 0, fn item, acc ->
            item.count + acc
          end)
          """,
          output: 10
        },
        {
          """
          Enum.map_reduce(cart, 0, fn item, acc ->
            {item.fruit, item.count + acc}
          end)
          """,
          output: {["apple", "banana", "orange"], 10}
        },
        {
          """
          Enum.scan(cart, 0, fn item, acc ->
            item.count + acc
          end)
          """,
          output: [3, 4, 10]
        },
        {
          """
          Enum.reduce_while(cart, 0, fn item, acc ->
            if item.fruit == "orange" do
              {:halt, acc}
            else
              {:cont, item.count + acc}
            end
          end)
          """,
          output: 4
        },
        {
          """
          for item <- cart, reduce: 0 do
            acc -> item.count + acc
          end
          """,
          output: 10
        },
        {
          """
          for item <- cart, item.fruit =~ "e", reduce: 0 do
            acc -> item.count + acc
          end
          """,
          output: 9, skip: true
        },
        {"Enum.count(cart)", output: 3},
        {
          ~s|Enum.frequencies(["apple", "banana", "orange", "apple"])|,
          output: %{"apple" => 2, "banana" => 1, "orange" => 1}
        },
        {"Enum.frequencies_by(cart, &String.last(&1.fruit))", output: %{"a" => 1, "e" => 2}},
        {~s|Enum.count(cart, &(&1.fruit =~ "e"))|, output: 2, skip: true},
        {~s|Enum.count(cart, &(&1.fruit =~ "y"))|, output: 0, skip: true},
        {"cart |> Enum.map(& &1.count) |> Enum.sum()", output: 10},
        {"Enum.sum_by(cart, & &1.count)", output: :todo, skip: true},
        {"cart |> Enum.map(& &1.count) |> Enum.product()", output: 18},
        {"Enum.product_by(cart, & &1.count)", output: :todo, skip: true},
        {"cart |> Enum.map(& &1.fruit) |> Enum.sort()", output: ["apple", "banana", "orange"]},
        {
          "cart |> Enum.map(& &1.fruit) |> Enum.sort(:desc)",
          output: ["orange", "banana", "apple"]
        },
        {
          "Enum.sort_by(cart, & &1.count)",
          output: [
            %{fruit: "banana", count: 1},
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          "Enum.sort_by(cart, & &1.count, :desc)",
          output: [
            %{fruit: "orange", count: 6},
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1}
          ]
        },
        {"cart |> Enum.map(& &1.count) |> Enum.min()", output: 1},
        {"Enum.min_by(cart, & &1.count)", output: :todo, skip: true},
        {"cart |> Enum.map(& &1.count) |> Enum.max()", output: 6},
        {"Enum.max_by(cart, & &1.count)", output: :todo, skip: true},
        {"Enum.concat([[1, 2, 3], [4, 5, 6], [7, 8, 9]])", output: [1, 2, 3, 4, 5, 6, 7, 8, 9]},
        {"Enum.concat([1, 2, 3], [4, 5, 6])", output: [1, 2, 3, 4, 5, 6]},
        {
          """
          Enum.flat_map(cart, fn item ->
            List.duplicate(item.fruit, item.count)
          end)
          """,
          output: [
            "apple",
            "apple",
            "apple",
            "banana",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange"
          ]
        },
        {
          """
          Enum.flat_map_reduce(cart, 0, fn item, acc ->
            list = List.duplicate(item.fruit, item.count)
            acc = acc + item.count
            {list, acc}
          end)
          """,
          output:
            {[
               "apple",
               "apple",
               "apple",
               "banana",
               "orange",
               "orange",
               "orange",
               "orange",
               "orange",
               "orange"
             ], 10}
        },
        {
          """
          for item <- cart,
              fruit <- List.duplicate(item.fruit, item.count) do
            fruit
          end
          """,
          output: [
            "apple",
            "apple",
            "apple",
            "banana",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange",
            "orange"
          ]
        },
        {
          """
          Enum.into(cart, %{}, fn item ->
            {item.fruit, item.count}
          end)
          """,
          output: %{"apple" => 3, "banana" => 1, "orange" => 6}
        },
        {
          """
          for item <- cart, into: %{} do
            {item.fruit, item.count}
          end
          """,
          output: %{"apple" => 3, "banana" => 1, "orange" => 6}
        },
        {
          ~s|Enum.dedup_by(cart, & &1.fruit =~ "a")|,
          output: [%{fruit: "apple", count: 3}], skip: true
        },
        {
          "Enum.dedup_by(cart, & &1.count < 5)",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          "Enum.uniq_by(cart, &String.last(&1.fruit))",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1}
          ]
        },
        {"Enum.at(cart, 0)", output: %{fruit: "apple", count: 3}},
        {"Enum.at(cart, 10)", output: nil},
        {"Enum.at(cart, 10, :none)", output: :none},
        {"Enum.fetch(cart, 0)", output: {:ok, %{fruit: "apple", count: 3}}},
        {"Enum.fetch(cart, 10)", output: :error},
        {"Enum.fetch!(cart, 0)", output: %{fruit: "apple", count: 3}},
        {"Enum.fetch!(cart, 10)", raises: Enum.OutOfBoundsError},
        {
          "Enum.with_index(cart)",
          output: [
            {%{fruit: "apple", count: 3}, 0},
            {%{fruit: "banana", count: 1}, 1},
            {%{fruit: "orange", count: 6}, 2}
          ]
        },
        {
          """
          Enum.with_index(cart, fn item, index ->
            {item.fruit, index}
          end)
          """,
          output: [
            {"apple", 0},
            {"banana", 1},
            {"orange", 2}
          ]
        },
        {
          ~s|Enum.find(cart, &(&1.fruit =~ "o"))|,
          output: %{fruit: "orange", count: 6}, skip: true
        },
        {
          ~s|Enum.find(cart, &(&1.fruit =~ "y"))|,
          output: nil, skip: true
        },
        {
          ~s|Enum.find(cart, :none, &(&1.fruit =~ "y"))|,
          output: :none, skip: true
        },
        {
          ~s|Enum.find_index(cart, &(&1.fruit =~ "o"))|,
          output: 2, skip: true
        },
        {
          ~s|Enum.find_index(cart, &(&1.fruit =~ "y"))|,
          output: nil, skip: true
        },
        {
          """
          Enum.find_value(cart, fn item ->
            if item.count == 1, do: item.fruit, else: nil
          end)
          """,
          output: "banana"
        },
        {
          """
          Enum.find_value(cart, :none, fn item ->
            if item.count == 100, do: item.fruit, else: nil
          end)
          """,
          output: :none
        },
        {
          "Enum.group_by(cart, &String.last(&1.fruit))",
          output: %{
            "a" => [%{fruit: "banana", count: 1}],
            "e" => [
              %{fruit: "apple", count: 3},
              %{fruit: "orange", count: 6}
            ]
          }
        },
        {
          "Enum.group_by(cart, &String.last(&1.fruit), & &1.fruit)",
          output: %{
            "a" => ["banana"],
            "e" => ["apple", "orange"]
          }
        },
        {~s|Enum.map_join(cart, ", ", & &1.fruit)|, output: "apple, banana, orange"},
        {
          ~s|Enum.map_intersperse(cart, ", ", & &1.fruit)|,
          output: ["apple", ", ", "banana", ", ", "orange"]
        },
        {
          "Enum.slice(cart, 0..1)",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1}
          ]
        },
        {
          "Enum.slice(cart, -2..-1)",
          output: [
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          "Enum.slice(cart, 1, 2)",
          output: [
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          "Enum.reverse(cart)",
          output: [
            %{fruit: "orange", count: 6},
            %{fruit: "banana", count: 1},
            %{fruit: "apple", count: 3}
          ]
        },
        {
          "Enum.reverse(cart, [:this_will_be, :the_tail])",
          output: [
            %{fruit: "orange", count: 6},
            %{fruit: "banana", count: 1},
            %{fruit: "apple", count: 3},
            :this_will_be,
            :the_tail
          ]
        },
        {
          "Enum.reverse_slice(cart, 1, 2)",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6},
            %{fruit: "banana", count: 1}
          ]
        },
        {
          "Enum.split(cart, 1)",
          output: {
            [%{fruit: "apple", count: 3}],
            [%{fruit: "banana", count: 1}, %{fruit: "orange", count: 6}]
          }
        },
        {
          "Enum.split(cart, -1)",
          output: {
            [%{fruit: "apple", count: 3}, %{fruit: "banana", count: 1}],
            [%{fruit: "orange", count: 6}]
          }
        },
        {
          ~s|Enum.split_while(cart, &(&1.fruit =~ "e"))|,
          output: {
            [%{fruit: "apple", count: 3}],
            [%{fruit: "banana", count: 1}, %{fruit: "orange", count: 6}]
          },
          skip: true
        },
        {
          ~s|Enum.split_with(cart, &(&1.fruit =~ "e"))|,
          output: {
            [%{fruit: "apple", count: 3}, %{fruit: "orange", count: 6}],
            [%{fruit: "banana", count: 1}]
          },
          skip: true
        },
        {
          "Enum.drop(cart, 1)",
          output: [
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          "Enum.drop(cart, -1)",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1}
          ]
        },
        {
          "Enum.drop_every(cart, 2)",
          output: [%{fruit: "banana", count: 1}]
        },
        {
          ~s|Enum.drop_while(cart, &(&1.fruit =~ "e"))|,
          output: [
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 6}
          ],
          skip: true
        },
        {"Enum.take(cart, 1)", output: [%{fruit: "apple", count: 3}]},
        {"Enum.take(cart, -1)", output: [%{fruit: "orange", count: 6}]},
        {
          "Enum.take_every(cart, 2)",
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          ~s|Enum.take_while(cart, &(&1.fruit =~ "e"))|,
          output: [%{fruit: "apple", count: 3}], skip: true
        },
        {
          "Enum.chunk_by(cart, &String.length(&1.fruit))",
          output: [
            [%{fruit: "apple", count: 3}],
            [%{fruit: "banana", count: 1}, %{fruit: "orange", count: 6}]
          ]
        },
        {
          "Enum.chunk_every(cart, 2)",
          output: [
            [%{fruit: "apple", count: 3}, %{fruit: "banana", count: 1}],
            [%{fruit: "orange", count: 6}]
          ]
        },
        {
          "Enum.chunk_every(cart, 2, 2, [:elements, :to_complete])",
          output: [
            [%{fruit: "apple", count: 3}, %{fruit: "banana", count: 1}],
            [%{fruit: "orange", count: 6}, :elements]
          ]
        },
        {
          "Enum.chunk_every(cart, 2, 1, :discard)",
          output: [
            [%{fruit: "apple", count: 3}, %{fruit: "banana", count: 1}],
            [%{fruit: "banana", count: 1}, %{fruit: "orange", count: 6}]
          ]
        },
        {
          "cart |> Enum.map(&{&1.fruit, &1.count}) |> Enum.unzip()",
          output: {["apple", "banana", "orange"], [3, 1, 6]}
        }
      ]
    },
    {
      """
      fruits = ["apple", "banana", "grape", "orange", "pear"]
      """,
      cases: [
        {
          "Enum.slide(fruits, 2, 0)",
          output: ["grape", "apple", "banana", "orange", "pear"], skip: true
        },
        {
          "Enum.slide(fruits, 2, 4)",
          output: ["apple", "banana", "orange", "pear", "grape"], skip: true
        },
        {"Enum.slide(fruits, 1..3, 0)", output: ["banana", "grape", "orange", "apple", "pear"]},
        {"Enum.slide(fruits, 1..3, 4)", output: ["apple", "pear", "banana", "grape", "orange"]}
      ]
    },
    {
      """
      fruits = ["apple", "banana", "orange"]
      counts = [3, 1, 6]
      """,
      cases: [
        {"Enum.zip(fruits, counts)", output: [{"apple", 3}, {"banana", 1}, {"orange", 6}]},
        {
          """
          Enum.zip_with(fruits, counts, fn fruit, count ->
            %{fruit: fruit, count: count}
          end)
          """,
          output: [
            %{fruit: "apple", count: 3},
            %{fruit: "banana", count: 1},
            %{fruit: "orange", count: 6}
          ]
        },
        {
          """
          Enum.zip_reduce(fruits, counts, 0, fn fruit, count, acc ->
            price = if fruit =~ "e", do: count * 2, else: count
            acc + price
          end)
          """,
          output: 19, skip: true
        }
      ]
    },
    {~s|Enum.any?([], & &1.fruit == "orange")|, output: false},
    {"Enum.all?([], & &1.count > 0)", output: true},
    {"Enum.empty?([])", output: true},
    {
      """
      pairs = [{"apple", 3}, {"banana", 1}, {"orange", 6}]
      Enum.into(pairs, %{})
      """,
      output: %{"apple" => 3, "banana" => 1, "orange" => 6}
    },
    {"Enum.to_list(1..5)", output: [1, 2, 3, 4, 5]},
    {"Enum.dedup([1, 2, 2, 3, 3, 3, 1, 2, 3])", output: [1, 2, 3, 1, 2, 3]},
    {"Enum.uniq([1, 2, 2, 3, 3, 3, 1, 2, 3])", output: [1, 2, 3]},
    {~s|Enum.join(["apple", "banana", "orange"], ", ")|, output: "apple, banana, orange"},
    {
      ~s|Enum.intersperse(["apple", "banana", "orange"], ", ")|,
      output: ["apple", ", ", "banana", ", ", "orange"]
    }
  ]
  |> create_tests(tag: :enum_cheatsheet)
end
