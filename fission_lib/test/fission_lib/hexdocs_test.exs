defmodule FissionLib.HexdocsTestHelper do
  use ExUnit.Case, async: true
  alias FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest

  defmacro assert_eval(code_string, expected) do
    quote do
      async_test "evaluation", %{tmp_dir: dir} do
        result =
          (@additional_prep <> unquote(code_string))
          |> AtomVM.eval(:elixir, run_dir: dir)
          |> unquote(__MODULE__).assert_expectation(unquote(expected))
      end
    end
  end

  defmacro assert_eval_module(code_string) do
    quote do
      async_test "evaluation", %{tmp_dir: dir} do
        result =
          (@additional_prep <> unquote(code_string))
          |> AtomVM.eval(:elixir, run_dir: dir)
          |> AtomVM.assert_is_module()
      end
    end
  end
  
  defmacro assert_error(code_string, expected_error) do
    quote do
      async_test "evaluation", %{tmp_dir: dir} do
        result =
          "try do\n" <>
          (@additional_prep <> unquote(code_string)) <>
          """
          rescue
            e -> e
          end
          """
          |> AtomVM.eval(:elixir, run_dir: dir)
        assert unquote(expected_error) = result
      end
    end
  end

  def assert_expectation(result, {:expect_fn, f}) when is_function(f) do
    assert f.(result)
  end

  def assert_expectation(result, e) do
    assert e === result
  end
end

defmodule FissionLib.HexdocsTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.HexdocsTestHelper

  @moduletag :tmp_dir
  @additional_prep ""

  # =======================================================================================================================
  # Introduction =========================================================================================================
  # =======================================================================================================================

  assert_eval("40 + 2\n", 42)

  """
  "hello" <> " world"
  """
  |> assert_eval("hello world")

  # =======================================================================================================================
  # Basic types ==========================================================================================================
  # =======================================================================================================================

  assert_eval("1\n", {:expect_fn, &is_integer/1})
  assert_eval("1\n", {:expect_fn, &is_integer/1})
  assert_eval("0x1F\n", {:expect_fn, &is_integer/1})
  assert_eval("1.0\n", {:expect_fn, &is_float/1})
  assert_eval("true\n", {:expect_fn, &is_boolean/1})
  assert_eval(":atom\n", {:expect_fn, &is_atom/1})
  assert_eval("\"elixir\"\n", {:expect_fn, &String.valid?/1})
  assert_eval("[1, 2, 3]\n", {:expect_fn, &is_list/1})
  assert_eval("{1, 2, 3}\n", {:expect_fn, &is_tuple/1})

  assert_eval("1 + 2\n", 3)
  assert_eval("5 * 5\n", 25)
  assert_eval("10 / 2\n", 5.0)

  assert_eval("div(10, 2)\n", 5)
  assert_eval("div 10, 2\n", 5)
  assert_eval("rem 10, 3\n", 1)

  assert_eval("0b1010\n", 10)
  assert_eval("0o777\n", 511)
  assert_eval("0x1F\n", 31)
  assert_eval("1.0\n", 1.0)
  assert_eval("1.0e-10\n", 1.0e-10)

  assert_eval("round(3.58)\n", 4)
  assert_eval("trunc(3.58)\n", 3)

  assert_eval("is_integer(1)\n", true)
  assert_eval("is_integer(2.0)\n", false)

  assert_eval("true\n", true)
  assert_eval("true == false\n", false)

  assert_eval("true and true\n", true)
  assert_eval("false or is_boolean(true)\n", true)

  assert_error("1 and true\n", %BadBooleanError{})

  assert_eval("false and raise(\"This error will never be raised\")\n", false)
  assert_eval("true or raise(\"This error will never be raised\")\n", true)

  assert_eval("1 || true\n", 1)
  assert_eval("false || 11\n", 11)
  assert_eval("nil && 13\n", nil)
  assert_eval("true && 17\n", 17)

  assert_eval("!true\n", false)
  assert_eval("!1\n", false)
  assert_eval("!nil\n", true)
  assert_eval(":apple\n", :apple)
  assert_eval(":orange\n", :orange)
  assert_eval(":watermelon\n", :watermelon)

  assert_eval(":apple == :apple\n", true)
  assert_eval(":apple == :orange\n", false)

  assert_eval("true == :true\n", true)
  assert_eval("is_atom(false)\n", true)
  assert_eval("is_boolean(:false)\n", true)

  assert_eval("\"hellö\"", "hellö")

  assert_eval("\"hello \" <> \"world!\"", "hello world!")
  
  """
  string = "world"
  "hello \#{string}!"
  """
  |> assert_eval("hello world!")

  """
  number = 42
  "i am \#{number} years old!"
  """
  |> assert_eval("i am 42 years old!")
  
  """
  \"hello
  world\"
  """
  |> assert_eval("hello\nworld")

  assert_eval("\"hello\nworld\"\n", "hello\nworld")

  assert_eval("IO.puts(\"hello\nworld\")", :ok)

  assert_eval("is_binary(\"hellö\")\n", true)

  assert_eval("byte_size(\"hellö\")\n", 6)

  assert_eval("String.length(\"hellö\")\n", 5)

  assert_eval("String.upcase(\"hellö\")\n", "HELLÖ")

  assert_eval("1 == 1\n", true)
  assert_eval("1 != 2\n", true)
  assert_eval("1 < 2\n", true)

  assert_eval("\"foo\" == \"foo\"\n", true)
  assert_eval("\"foo\" == \"bar\"\n", false)

  assert_eval("1 == 1.0\n", true)
  assert_eval("1 == 2.0\n", false)

  assert_eval("1 === 1.0\n", false)

  # =======================================================================================================================
  # Lists and tuples =====================================================================================================
  # =======================================================================================================================

  assert_eval("[1, 2, true, 3]\n", [1, 2, true, 3])
  assert_eval("length([1, 2, 3])\n", 3)

  assert_eval("[1, 2, 3] ++ [4, 5, 6]\n", [1, 2, 3, 4, 5, 6])
  #  todo 6 subtraction of two lists with different type terms inside is failing
  #  assert_eval("[1, true, 2, false, 3, true] -- [true, false]\n", [1, 2, 3, true])

  """
  list = [1, 2, 3]
  hd(list)
  """
  |> assert_eval(1)

  """
  list = [1, 2, 3]
  hd(list)
  tl(list)
  """
  |> assert_eval([2, 3])

  assert_error("hd([])\n", %ArgumentError{})
  
  assert_eval("[11, 12, 13]\n", ~c"\v\f\r")
  assert_eval("[104, 101, 108, 108, 111]\n", ~c"hello")

  assert_eval("{:ok, \"hello\"}\n", {:ok, "hello"})
  assert_eval("tuple_size({:ok, \"hello\"})", 2)

  """
  tuple = {:ok, "hello"}
  elem(tuple, 1)
  tuple_size(tuple)
  """
  |> assert_eval(2)

  """
  tuple = {:ok, "hello"}
  put_elem(tuple, 1, "world")
  tuple
  """
  |> assert_eval({:ok, "hello"})

  """
  tuple = {:ok, "hello"}
  put_elem(tuple, 1, "world")
  """
  |> assert_eval({:ok, "world"})

  """
  list = [1, 2, 3]
  # This is fast as we only need to traverse `[0]` to prepend to `list`
  [0] ++ list
  """
  |> assert_eval([0, 1, 2, 3])

  """
  list = [1, 2, 3]
  # This is slow as we need to traverse `list` to append 4
  list ++ [4]
  """
  |> assert_eval([1, 2, 3, 4])

  """
  tuple = {:a, :b, :c, :d}
  put_elem(tuple, 2, :e)
  """
  |> assert_eval({:a, :b, :e, :d})

  #  todo 7 String.split is failing
  #  assert_eval("String.split(\"hello world\")\n", ["hello", "world"])
  #  assert_eval("String.split(\"hello beautiful world\")\n", ["hello", "beautiful", "world"])

  assert_eval("String.split_at(\"hello world\", 3)", {"hel", "lo world"})
  assert_eval("String.split_at(\"hello world\", -4)", {"hello w", "orld"})

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  assert_eval("File.read(\"path/to/existing/file\")", {:ok, "... contents ..."})
  #  assert_eval("File.read(\"path/to/unknown/file\")", {:error, :enoent})
  """
  tuple = {:ok, "hello"}
  elem(tuple, 1)
  """
  |> assert_eval("hello")

  # =======================================================================================================================
  # Pattern matching =====================================================================================================
  # =======================================================================================================================
  """
  x = 1
  x
  """
  |> assert_eval(1)

  """
  x = 1
  1 = x
  2 = x
  """
  |> assert_error(%MatchError{})
  
  """
  {a, b, c} = {:hello, "world", 42}
  a
  """
  |> assert_eval(:hello)

  """
  {a, b, c} = {:hello, "world", 42}
  b
  """
  |> assert_eval("world")

  """
  {a, b, c} = {:hello, "world"}
  """
  |> assert_error(%MatchError{})

  """
  {a, b, c} = [:hello, "world", 42]
  """
  |> assert_error(%MatchError{})

  """
  {:ok, result} = {:ok, 13}
  result
  """
  |> assert_eval(13)

  """
  {:ok, result} = {:ok, 13}
  {:ok, result} = {:error, :oops}
  """
  |> assert_error(%MatchError{})

  """
  [a, b, c] = [1, 2, 3]
  a
  """
  |> assert_eval(1)

  """
  [head | tail] = [1, 2, 3]
  head
  """
  |> assert_eval(1)

  """
  [head | tail] = [1, 2, 3]
  tail
  """
  |> assert_eval([2, 3])

  @tag :skip
  """
  [head | tail] = []
  """
  |> assert_error(%MatchError{})

  """
  list = [1, 2, 3]
  [0 | list]
  """
  |> assert_eval([0, 1, 2, 3])

  """
  x = 1
  x = 2
  """
  |> assert_eval(2)

  """
  x = 1
  ^x = 2
  """
  |> assert_error(%MatchError{})

  """
  1 = 2
  """
  |> assert_error(%MatchError{})

  """
  x = 1
  [^x, 2, 3] = [1, 2, 3]
  {y, ^x} = {2, 1}
  y
  {y, ^x} = {2, 2}
  """
  |> assert_error(%MatchError{})
  """
  x = 1
  [^x, 2, 3] = [1, 2, 3]
  {y, ^x} = {2, 1}
  y
  """
  |> assert_eval(2)

  """
  x = 1
  [^x, 2, 3] = [1, 2, 3]
  {y, ^x} = {2, 1}
  """
  |> assert_eval({2, 1})

  """
  x = 1
  [^x, 2, 3] = [1, 2, 3]
  """
  |> assert_eval([1, 2, 3])
  
  @tag :skip
  """
  {y, 1} = {2, 2}
  """
  |> assert_error(%MatchError{})
  
  """
  [head | _] = [1, 2, 3]
  head
  """
  |> assert_eval(1)

  # =======================================================================================================================
  # case, cond, and if ===================================================================================================
  # =======================================================================================================================

  #  todo 8 Unused variables causes eval to fail (in the following "_x" works just fine)
  #  """
  #  case {1, 2, 3} do
  #    {1, x, 3} ->
  #      "This clause will match and bind x to 2 in this clause"
  #    {4, 5, 6} ->
  #      "This clause won't match"
  #    _ ->
  #      "This clause would match any value"
  #  end
  #  """
  #  |> assert_eval("This clause will match and bind x to 2 in this clause")

  """
  x = 1
  case 10 do
    ^x -> "Won't match"
    _ -> "Will match"
  end
  """
  |> assert_eval("Will match")

  """
  case {1, 2, 3} do
    {1, x, 3} when x > 0 ->
      "Will match"
    _ ->
      "Would match, if guard condition were not satisfied"
  end
  """
  |> assert_eval("Will match")

  """
  hd(1)
  """
  |> assert_error(%ArgumentError{})

  #  """
  #  case 1 do
  #    x when hd(x) -> "Won't match"
  #    x -> "Got \#\{x\}"
  #  end
  #  """
  #  |> assert_eval("Got 1")

  """
  case :ok do
    :error -> "Won't match"
  end
  """
  |> assert_error(%CaseClauseError{})

  """
  if true do
    "This works!"
  end
  """
  |> assert_eval("This works!")

  """
  if false do
    "This will never be seen"
  end
  """
  |> assert_eval(nil)

  #  todo 9 fix if statement (if nil is not working as expected)
  #  """
  #  if nil do
  #    "This won't be seen"
  #  else
  #    "This will"
  #  end
  #  """
  #  |> assert_eval(nil)

  #  todo 9 fix if with match inside (this could be caused by 8)
  #  """
  #  x = 1
  #  if true do
  #    x = x + 1
  #  end
  #  """
  #  |> assert_eval(2)

  #  """
  #  x = 1
  #  if true do
  #    x = x + 1
  #  end
  #  x
  #  """
  #  |> assert_eval(1)

  """
  x = 1
  x = if true do
    x + 1
  else
    x
  end
  """
  |> assert_eval(2)

  """
  cond do
    2 + 2 == 5 ->
      "This is never true"
    2 * 2 == 3 ->
      "Nor this"
    true ->
      "This is always true (equivalent to else)"
  end
  """
  |> assert_eval("This is always true (equivalent to else)")

  """
  cond do
    hd([1, 2, 3]) ->
      "1 is considered as true"
  end
  """
  |> assert_eval("1 is considered as true")

  # =======================================================================================================================
  # Anonymous functions ==================================================================================================
  # =======================================================================================================================

  """
  add = fn a, b -> a + b end
  add.(1, 2)
  """
  |> assert_eval(3)

  """
  add = fn a, b -> a + b end
  is_function(add)
  """
  |> assert_eval(true)

  #  todo 10 is_function/2 not implemented
  #  """
  #  add = fn a, b -> a + b end
  #  is_function(add, 2)
  #  """
  #  |> assert_eval(true)

  #  """
  #  add = fn a, b -> a + b end
  #  is_function(add, 1)
  #  """
  #  |> assert_eval(false)
  """
  add = fn a, b -> a + b end
  double = fn a -> add.(a, a) end
  double.(2)
  """
  |> assert_eval(4)

  #  todo 11 specific evaluation of anonymous function does not work (possibly because of zero arguments)
  #  """
  #  x = 42
  #  (fn -> x = 0 end).()
  #  """
  #  |> assert_eval(0)

  #  """
  #  x = 42
  #  (fn -> x = 0 end).()
  #  x
  #  """
  #  |> assert_eval(42)

  """
  f = fn
    x, y when x > 0 -> x + y
    x, y -> x * y
  end
  f.(1, 3)
  """
  |> assert_eval(4)

  """
  f = fn
    x, y when x > 0 -> x + y
    x, y -> x * y
  end
  f.(-1, 3)
  """
  |> assert_eval(-3)

  """
  fun = &is_atom/1
  """
  |> assert_eval(&:erlang.is_atom/1)

  """
  fun = &is_atom/1
  is_function(fun)
  """
  |> assert_eval(true)

  #  todo 12 guards as anonymous functions are not working (adding module to the function [Kernel. or :erlang.] does not help)
  #  """
  #  fun = &is_atom/1
  #  :erlang.is_atom(:hello)
  #  """
  #  |> assert_eval(true)

  #  """
  #  fun = &is_atom/1
  #  fun.(123)
  #  """
  #  |> assert_eval(false)

  """
  fun = &String.length/1
  """
  |> assert_eval(&String.length/1)

  """
  fun = &String.length/1
  fun.("hello")
  """
  |> assert_eval(5)

  """
  add = &+/2
  """
  |> assert_eval(&:erlang.+/2)

  #  todo 13 :erlang.+/2 is not working as an anonymous function
  #  """
  #  add = &+/2
  #  add.(1, 2)
  #  """
  #  |> assert_eval(3)

  #  """
  #  add = &+/2
  #  is_arity_2 = fn fun -> is_function(fun, 2) end
  #  is_arity_2.(add)
  #  """
  #  |> assert_eval(true)

  """
  fun = &(&1 + 1)
  fun.(1)
  """
  |> assert_eval(2)

  """
  fun2 = &"Good \#\{&1\}"
  fun2.("morning")
  """
  |> assert_eval("Good morning")

  # =======================================================================================================================
  # Binaries, strings, and charlists =====================================================================================
  # =======================================================================================================================

  """
  string = "hello"
  is_binary(string)
  """
  |> assert_eval(true)

  assert_eval("?a\n", 97)
  assert_eval("?ł\n", 322)

  """
  "\u0061" == "a"
  """
  |> assert_eval(true)

  """
  0x0061 = 97 = ?a
  """
  |> assert_eval(97)

  """
  string = "héllo"
  String.length(string)
  """
  |> assert_eval(5)

  """
  string = "héllo"
  byte_size(string)
  """
  |> assert_eval(6)

  """
  String.codepoints("👩‍🚒")
  ["👩", "‍", "🚒"]
  """
  |> assert_eval(["👩", "‍", "🚒"])

  #  todo 13 String.graphemes/1 does not work for fireman emote
  #  """
  #  String.graphemes("👩‍🚒")
  #  """
  #  |> assert_eval(["👩‍🚒"])

  """
  String.length("👩‍🚒")
  """
  |> assert_eval(1)

  """
  "hełło" <> <<0>>
  """
  |> assert_eval(<<104, 101, 197, 130, 197, 130, 111, 0>>)

  #  todo 14 IO.inspect with option :binaries fails with function_clause error
  #  """
  #  IO.inspect("hełło", binaries: :as_binaries)
  #  """
  #  |> assert_eval(<<104, 101, 197, 130, 197, 130, 111>>)

  #  todo 15 "::" does not work for binaries
  assert_eval("<<42>> == <<42::8>>\n", true)
  #  assert_eval("<<3::4>>\n", <<3::size(4)>>)
  #  assert_eval("<<0::1, 0::1, 1::1, 1::1>> == <<3::4>>\n", true)
  #  assert_eval("is_bitstring(<<3::4>>)\n", true)
  #  assert_eval("is_binary(<<3::4>>)\n", false)
  assert_eval("is_bitstring(<<0, 255, 42>>)\n", true)
  assert_eval("is_binary(<<0, 255, 42>>)\n", true)
  #  assert_eval("is_binary(<<42::16>>)\n", true)

  """
  <<0, 1, x>> = <<0, 1, 2>>
  x
  """
  |> assert_eval(2)

  """
  <<0, 1, x>> = <<0, 1, 2, 3>>
  """
  |> assert_error(%MatchError{})

  #  todo 15 "::" does not work for binaries
  #  """
  #  <<head::binary-size(2), rest::binary>> = <<0, 1, 2, 3>>
  #  head
  #  """
  #  |> assert_eval(<<0, 1>>)

  #  """
  #  <<head::binary-size(2), rest::binary>> = <<0, 1, 2, 3>>
  #  rest
  #  """
  #  |> assert_eval(<<2, 3>>)

  assert_eval("is_binary(\"hello\")\n", true)
  assert_eval("is_binary(<<239, 191, 19>>)\n", true)
  assert_eval("String.valid?(<<239, 191, 19>>)\n", false)

  """
  "a" <> "ha"
  """
  |> assert_eval("aha")

  assert_eval("<<0, 1>> <> <<2, 3>>\n", <<0, 1, 2, 3>>)

  """
  <<head, rest::binary>> = "banana"
  head == ?b
  """
  |> assert_eval(true)

  """
  <<head, rest::binary>> = "banana"
  rest
  """
  |> assert_eval("anana")

  """
  "ü" <> <<0>>
  """
  |> assert_eval(<<195, 188, 0>>)

  """
  <<x, rest::binary>> = "über"
  x == ?ü
  """
  |> assert_eval(false)

  """
  <<x, rest::binary>> = "über"
  rest
  """
  |> assert_eval(<<188, 98, 101, 114>>)

  """
  <<x::utf8, rest::binary>> = "über"
  x == ?ü
  """
  |> assert_eval(true)

  """
  <<x::utf8, rest::binary>> = "über"
  rest
  """
  |> assert_eval("ber")

  """
  ~c"hello"
  """
  |> assert_eval(~c"hello")

  """
  [?h, ?e, ?l, ?l, ?o]
  """
  |> assert_eval(~c"hello")

  """
  ~c"hełło"
  """
  |> assert_eval([104, 101, 322, 322, 111])

  """
  is_list(~c"hełło")
  """
  |> assert_eval(true)

  """
  heartbeats_per_minute = [99, 97, 116]
  """
  |> assert_eval(~c"cat")

  #  todo 16 inspect/2 returns "'cat'" instead of "[99, 97, 116]"
  #  """
  #  heartbeats_per_minute = [99, 97, 116]
  #  inspect(heartbeats_per_minute, charlists: :as_list)
  #  """
  #  |> assert_eval("[99, 97, 116]")
  """
  to_charlist("hełło")
  """
  |> assert_eval([104, 101, 322, 322, 111])

  #  todo 17 to_string/1 "heBBo" instead of "hełło"
  #  """
  #  to_string(~c"hełło")
  #  """
  #  |> assert_eval("hełło")

  """
  to_string(:hello)
  """
  |> assert_eval("hello")

  """
  to_string(1)
  """
  |> assert_eval("1")

  #  todo 40 concat does not returning ArgumentError with sigil c
  #  """
  #  ~c"this " <> ~c"fails"
  #  """
  #  |> assert_error(%ArgumentError{})
  
  """
  ~c"this " ++ ~c"works"
  """
  |> assert_eval(~c"this works")

  #  """
  #  "he" ++ "llo"
  #  """
  #  |> assert_argument_error()
  """
  "he" <> "llo"
  """
  |> assert_eval("hello")

  # =======================================================================================================================
  # Keyword lists and maps ===============================================================================================
  # =======================================================================================================================

  """
  String.split("1 2 3 4", " ")
  """
  |> assert_eval(["1", "2", "3", "4"])

  #  todo 18 Keyword does not work in option for String.split/3
  #  """
  #  String.split("1 2 3 4", " ", [parts: 3])
  #  """
  #  |> assert_eval(["1", "2", "3 4"])

  #  """
  #  String.split("1  2  3  4", " ", [parts: 3])
  #  """
  #  |> assert_eval(["1", "", "2  3  4"])

  #  """
  #  String.split("1  2  3  4", " ", [parts: 3, trim: true])
  #  """
  #  |> assert_eval(["1", "2", " 3  4"])

  #  """
  #  String.split("1  2  3  4", " ", parts: 3, trim: true)
  #  """
  #  |> assert_eval(["1", "2", " 3  4"])

  """
  [{:parts, 3}, {:trim, true}] == [parts: 3, trim: true]
  """
  |> assert_eval(true)

  #  """
  #  import String, only: [split: 1, split: 2]
  #  split("hello world")
  #  """
  #  |> assert_eval(["hello", "world"])

  """
  list = [a: 1, b: 2]
  list ++ [c: 3]
  """
  |> assert_eval(a: 1, b: 2, c: 3)

  """
  list = [a: 1, b: 2]
  [a: 0] ++ list
  """
  |> assert_eval(a: 0, a: 1, b: 2)

  """
  list = [a: 1, b: 2]
  list[:a]
  """
  |> assert_eval(1)

  """
  list = [a: 1, b: 2]
  list[:b]
  """
  |> assert_eval(2)

  """
  [a: a] = [a: 1]
  a
  """
  |> assert_eval(1)
  
  """
  [a: a] = [a: 1, b: 2]
  """
  |> assert_error(%MatchError{})

  """
  [b: b, a: a] = [a: 1, b: 2]
  """
  |> assert_error(%MatchError{})

  """
  if true do
     "This will be seen"
  else
    "This won't"
  end
  """
  |> assert_eval("This will be seen")

  """
  if true, do: "This will be seen", else: "This won't"
  """
  |> assert_eval("This will be seen")

  """
  map = %{:a => 1, 2 => :b}
  map[:a]
  """
  |> assert_eval(1)

  """
  map = %{:a => 1, 2 => :b}
  map[2]
  """
  |> assert_eval(:b)

  """
  map = %{:a => 1, 2 => :b}
  map[:c]
  """
  |> assert_eval(nil)

  """
  %{} = %{:a => 1, 2 => :b}
  """
  |> assert_eval(%{2 => :b, :a => 1})

  """
  %{:a => a} = %{:a => 1, 2 => :b}
  a
  """
  |> assert_eval(1)

  """
  %{:c => c} = %{:a => 1, 2 => :b}
  """
  |> assert_error(%MatchError{})

  """
  Map.get(%{:a => 1, 2 => :b}, :a)
  """
  |> assert_eval(1)

  """
  Map.put(%{:a => 1, 2 => :b}, :c, 3)
  """
  |> assert_eval(%{2 => :b, :a => 1, :c => 3})

  """
  Map.to_list(%{:a => 1, 2 => :b})
  """
  |> assert_eval([{2, :b}, {:a, 1}])

  """
  map = %{:name => "John", :age => 23}
  """
  |> assert_eval(%{name: "John", age: 23})

  @additional_prep """
  map = %{name: "John", age: 23}
  """
  
  """
  map.name
  """
  |> assert_eval("John")

  """
  map.agee
  """
  |> assert_error(%KeyError{})

  """
  %{map | name: "Mary"}
  """
  |> assert_eval(%{name: "Mary", age: 23})

  """
  %{map | agee: 27}
  """
  |> assert_error(%KeyError{})

  """
  users = [
    john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
    mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
  ]
  users[:john].age
  """
  |> assert_eval(27)

  """
  users = [
    john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
    mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
  ]
  users = put_in(users[:john].age, 31)
  """
  |> assert_eval(
    john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
    mary: %{age: 29, languages: ["Elixir", "F#", "Clojure"], name: "Mary"}
  )

  """
  users = [
    john: %{name: "John", age: 27, languages: ["Erlang", "Ruby", "Elixir"]},
    mary: %{name: "Mary", age: 29, languages: ["Elixir", "F#", "Clojure"]}
  ]
  users = put_in(users[:john].age, 31)
  users = update_in(users[:mary].languages, fn languages -> List.delete(languages, "Clojure") end)
  """
  |> assert_eval(
    john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
    mary: %{age: 29, languages: ["Elixir", "F#"], name: "Mary"}
  )

  # =======================================================================================================================
  # Modules and functions ================================================================================================
  # =======================================================================================================================

  """
  String.length("hello")
  """
  |> assert_eval(5)

  """
  defmodule Math do
    def sum(a, b) do
      a + b
    end
  end

  Math.sum(1, 2)
  """
  |> assert_eval(3)

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
  """
  |> assert_error(%UndefinedFunctionError{})

  """
  defmodule Math do
    def zero?(0) do
      true
    end

    def zero?(x) when is_integer(x) do
      false
    end
  end
  Math.zero?(0)
  """
  |> assert_eval(true)

  """
  defmodule Math do
    def zero?(0) do
      true
    end

    def zero?(x) when is_integer(x) do
      false
    end
  end

  Math.zero?(1)
  """
  |> assert_eval(false)

  """
  defmodule Math do
    def zero?(0) do
      true
    end

    def zero?(x) when is_integer(x) do
      false
    end
  end

  Math.zero?([1, 2, 3])
  """
  |> assert_error(%FunctionClauseError{})

  #  todo 41 error - unsupported
#  """
#  defmodule Math do
#    def zero?(0) do
#      true
#    end
#
#    def zero?(x) when is_integer(x) do
#      false
#    end
#  end
#
#  Math.zero?(0.0)
#  """
#  |> assert_error(%FunctionClauseError{})
  
  """
  defmodule Math do
    def zero?(0), do: true
    def zero?(x) when is_integer(x), do: false
  end
  """
  |> assert_eval_module()

  #  todo 19 "<>" does not work for variables - works only for plain strings
  #  todo 20 using default arguments defined with "\\" fails (use \\\\ inside the string code to test this behaviour)
  #  """
  #  defmodule Concat do
  #    def join(a, b, sep \\\\ " ") do
  #      {a, b, sep}
  #    end
  #  end
  #  Concat.join("Hello", "world")
  #  """
  #  |> assert_eval("Hello world")

  #  """
  #  defmodule Concat do
  #    def join(a, b, sep \\ " ") do
  #      a <> sep <> b
  #    end
  #  end
  #  Concat.join("Hello", "world", "_")
  #  """
  #  |> assert_eval("Hello_world")

  #  """
  #  defmodule DefaultTest do
  #    def dowork(x \\ "hello") do
  #      x
  #    end
  #  end
  #  DefaultTest.dowork()
  #  DefaultTest.dowork(123)
  #  DefaultTest.dowork()
  #  """
  #  |> assert_eval("hello")

  #  """
  #  defmodule DefaultTest do
  #    def dowork(x \\ "hello") do
  #      x
  #    end
  #  end
  #  DefaultTest.dowork(123)
  #  """
  #  |> assert_eval(123)

  #  """
  #  defmodule Concat do
  #    # A function head declaring defaults
  #    def join(a, b, sep \\ " ")
  #
  #    def join(a, b, _sep) when b == "" do
  #      a
  #    end
  #
  #    def join(a, b, sep) do
  #      a <> sep <> b
  #    end
  #  end
  #
  #  Concat.join("Hello", "")
  #  """
  #  |> assert_eval("Hello")

  #  """
  #  defmodule Concat do
  #    # A function head declaring defaults
  #    def join(a, b, sep \\ " ")
  #
  #    def join(a, b, _sep) when b == "" do
  #      a
  #    end
  #
  #    def join(a, b, sep) do
  #      a <> sep <> b
  #    end
  #  end
  #  Concat.join("Hello", "world"))
  #  """
  #  |> assert_eval("Hello world")

  #  """
  #  defmodule Concat do
  #    # A function head declaring defaults
  #    def join(a, b, sep \\ " ")
  #
  #    def join(a, b, _sep) when b == "" do
  #      a
  #    end
  #
  #    def join(a, b, sep) do
  #      a <> sep <> b
  #    end
  #  end
  #  Concat.join("Hello", "world", "_")
  #  """
  #  |> assert_eval("Hello_world")
  #  

  ## =======================================================================================================================
  ## Recursion ============================================================================================================
  ## =======================================================================================================================

  @additional_prep """
  defmodule Recursion do
    def print_multiple_times(msg, n) when n > 0 do
      IO.puts(msg)
      print_multiple_times(msg, n - 1)
    end

    def print_multiple_times(_msg, 0) do
      :ok
    end
  end
  """
  
  """
  Recursion.print_multiple_times("Hello!", 3)
  """
  |> assert_eval(:ok)

  """
  Recursion.print_multiple_times("Hello!", -1)
  """
  |> assert_error(%FunctionClauseError{})


  @additional_prep """
  defmodule Math do
    def sum_list([head | tail], accumulator) do
      sum_list(tail, head + accumulator)
    end

    def sum_list([], accumulator) do
      accumulator
    end
  end
  """
  
  """
  Math.sum_list([1, 2, 3], 0)
  """
  |> assert_eval(6)

  """
  Math.sum_list([2, 3], 1)
  """
  |> assert_eval(6)

  """
  Math.sum_list([3], 3)
  """
  |> assert_eval(6)

  """
  Math.sum_list([], 6)
  """
  |> assert_eval(6)

  @additional_prep ""

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
  """
  |> assert_eval([2, 4, 6])

  """
  Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)
  """
  |> assert_eval(6)

  """
  Enum.map([1, 2, 3], fn x -> x * 2 end)
  """
  |> assert_eval([2, 4, 6])

  """
  Enum.map([1, 2, 3], &(&1 * 2))
  """
  |> assert_eval([2, 4, 6])

  #  
  ## =======================================================================================================================
  ## Enumerables and Streams ==============================================================================================
  ## =======================================================================================================================

  """
  Enum.map(%{1 => 2, 3 => 4}, fn {k, v} -> k * v end)
  """
  |> assert_eval([2, 12])

  """
  Enum.map(1..3, fn x -> x * 2 end)
  """
  |> assert_eval([2, 4, 6])

  #  todo 13 :erlang.+/2 is not working as an anonymous function
  #  """
  #  Enum.reduce(1..3, 0, &+/2)
  #  """
  #  |> assert_eval(6)

  """
  odd? = fn x -> rem(x, 2) != 0 end
  Enum.filter(1..3, odd?)
  """
  |> assert_eval([1, 3])

  #  todo 21 the following freezes test execution
  #  """
  #  odd? = fn x -> rem(x, 2) != 0 end
  #  1..100_000 |> Enum.map(&(&1 * 3)) |> Enum.filter(odd?) |> Enum.sum()
  #  """
  #  |> assert_eval(7500000000)
  #
  #  """
  #  odd? = fn x -> rem(x, 2) != 0 end
  #  Enum.sum(Enum.filter(Enum.map(1..100_000, &(&1 * 3)), odd?))
  #  """
  #  |> assert_eval(7500000000)
  #
  #  """
  #  odd? = fn x -> rem(x, 2) != 0 end
  #  1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?) |> Enum.sum()
  #  """
  #  |> assert_eval(7500000000)

  #  todo 39 "Unknown external term type: <number>" when calling the Stream module functions
  #  """
  #  1..100_000 |> Stream.map(&(&1 * 3))
  #  """
  #  |> assert_eval({:expect_fn, fn term -> is_struct(term, Stream) end})
  #
  #  """
  #  1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?)
  #  """
  #  |> assert_eval({:expect_fn, fn term -> is_struct(term, Stream) end})
  #
  #  """
  #  stream = Stream.cycle([1, 2, 3])
  #  Enum.take(stream, 10)
  #  """
  #  |> assert_eval([1, 2, 3, 1, 2, 3, 1, 2, 3, 1])

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  "path/to/file" |> File.stream!() |> Enum.take(10)
  #  """
  #  |> assert_eval()

  ## =======================================================================================================================
  ## Processes ============================================================================================================
  ## =======================================================================================================================

  #  todo 22 spawning anonymous function and not assigned to a variable causes "unknown external type: 83" error
  #  """
  #  spawn(fn -> 1 + 2 end)
  #  """
  #  |> assert_eval({:expect_fn, &is_pid/1})

  #  todo 23 process with "pid" is still alive (Process.alive?/1 is true)
  #  """
  #  pid = spawn(fn -> 1 + 2 end)
  #  Process.alive?(pid)
  #  """
  #  |> assert_eval(false)

  """
  self()
  Process.alive?(self())
  """
  |> assert_eval(true)

  """
  send(self(), {:hello, "world"})
  receive do
    {:hello, msg} -> msg
    {:world, _msg} -> "won't match"
  end
  """
  |> assert_eval("world")

  """
  receive do
    {:hello, msg}  -> msg
  after
    1_000 -> "nothing after 1s"
  end
  """
  |> assert_eval("nothing after 1s")

  #  todo 22 spawning anonymous function and not assigned to a variable causes "unknown external type: 83" error
  #  """
  #  parent = self()
  #  spawn(fn -> send(parent, {:hello, self()}) end)
  #  receive do
  #    {:hello, pid} -> "Got hello from #{inspect pid}"
  #  end
  #  """
  #  |> assert_eval("Got hello from #PID<0.48.0>")

  #  """
  #  send(self(), :hello)
  #  flush()
  #  """
  #  |> assert_eval(:hello)
  
# todo - seems like the process is raising error in a good enough way but for some reason exception is not caught in trycatch
# todo - that is a behaviour exactly the same as in iex console so idk how to test it
#  """
#  spawn(fn -> raise "oops" end)
#  """
#  |> assert_error(%RuntimeError{})
#
#  """
#  spawn_link(fn -> raise "oops" end)
#  """
#  |> assert_error(%RuntimeError{})
#
#  """
#  Task.start(fn -> raise "oops" end)
#  """
#  |> assert_error(%RuntimeError{})

  #  todo 24 defining module that uses Task and trying to create such Task is causing compilation problems
  #  """
  #  defmodule KV do
  #    def start_link do
  #      Task.start_link(fn -> loop(%{}) end)
  #    end
  #
  #    defp loop(map) do
  #      receive do
  #        {:get, key, caller} ->
  #          send(caller, Map.get(map, key))
  #          loop(map)
  #        {:put, key, value} ->
  #          loop(Map.put(map, key, value))
  #      end
  #    end
  #  end
  #  {:ok, pid} = KV.start_link()
  #  send(pid, {:get, :hello, self()})
  #  flush()
  #  """
  #  |> assert_eval(nil)

  #  todo 24 - the "pid" variable in the following is the "pid" variable from the former
  #  """
  #  send(pid, {:put, :hello, :world})
  #  {:put, :hello, :world}
  #  send(pid, {:get, :hello, self()})
  #  {:get, :hello, #PID<0.41.0>}
  #  flush()
  #  :world
  #  :ok
  #  """
  #  |> assert_eval()
  #
  #  """
  #  Process.register(pid, :kv)
  #  true
  #  send(:kv, {:get, :hello, self()})
  #  {:get, :hello, #PID<0.41.0>}
  #  flush()
  #  :world
  #  :ok
  #  """
  #  |> assert_eval()

  #  todo 25 using Agent is causing the evaluation to fail
  #  """
  #  {:ok, pid} = Agent.start_link(fn -> %{} end)
  #  Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
  #  Agent.get(pid, fn map -> Map.get(map, :hello) end)
  #  """
  #  |> assert_eval(:world)

  #
  ## =======================================================================================================================
  ## IO and the file system ===============================================================================================
  ## =======================================================================================================================

  #  todo 38 testing IO - take result from try_run and output and assert it
  #  """
  #  IO.puts("hello world")
  #  hello world
  #  :ok
  #  IO.gets("yes or no? ")
  #  yes or no? yes
  #  "yes\n"
  #  """
  #  |> assert_eval()

  """
  IO.puts(:stderr, "hello world")
  """
  |> assert_eval(:ok)

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  {:ok, file} = File.open("path/to/file/hello", [:write])
  #  {:ok, #PID<0.47.0>}
  #  IO.binwrite(file, "world")
  #  :ok
  #  File.close(file)
  #  :ok
  #  File.read("path/to/file/hello")
  #  {:ok, "world"}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  File.read("path/to/file/hello")
  #  {:ok, "world"}
  #  File.read!("path/to/file/hello")
  #  "world"
  #  File.read("path/to/file/unknown")
  #  {:error, :enoent}
  #  File.read!("path/to/file/unknown")
  #  ** (File.Error) could not read file "path/to/file/unknown": no such file or directory
  #  """
  #  |> assert_eval()
  #
  #  """
  #  case File.read("path/to/file/hello") do
  #    {:ok, body} -> # do something with the `body`
  #    {:error, reason} -> # handle the error caused by `reason`
  #  end
  #  """
  #  |> assert_eval()

  #  """
  #  {:ok, body} = File.read("path/to/file/unknown")
  #  """
  #  |> assert_eval()

  """
  Path.join("foo", "bar")
  """
  |> assert_eval("foo/bar")

  #  """
  #  Path.expand("~/hello")
  #  """
  #  |> assert_eval("/Users/jose/hello")
  #
  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  {:ok, file} = File.open("hello")
  #  {:ok, #PID<0.47.0>}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  File.close(file)
  #  :ok
  #  IO.write(file, "is anybody out there")
  #  ** (ErlangError) Erlang error: :terminated:
  #
  #    * 1st argument: the device has terminated
  #
  #      (stdlib 5.0) io.erl:94: :io.put_chars(#PID<0.114.0>, "is anybody out there")
  #      iex:4: (file)
  #  """
  #  |> assert_eval()
  #
  #  """
  #  pid = spawn(fn ->
  #    receive do
  #      msg -> IO.inspect(msg)
  #    end
  #  end)
  #  #PID<0.57.0>
  #  IO.write(pid, "hello")
  #  {:io_request, #PID<0.41.0>, #Reference<0.0.8.91>,
  #   {:put_chars, :unicode, "hello"}}
  #  ** (ErlangError) erlang error: :terminated
  #  """
  #  |> assert_eval()
  #
  # todo 19 "<>" does not work for variables - works only for plain strings
  #  """
  #  name = "Mary"
  #  IO.puts("Hello " <> name <> "!")
  #  """
  #  |> assert_eval(:ok)

  """
  name = "Mary"
  IO.puts(["Hello ", name, "!"])
  """
  |> assert_eval(:ok)

  """
  Enum.join(["apple", "banana", "lemon"], ",")
  """
  |> assert_eval("apple,banana,lemon")

  """
  Enum.intersperse(["apple", "banana", "lemon"], ",")
  """
  |> assert_eval(["apple", ",", "banana", ",", "lemon"])

  """
  IO.puts(["apple", [",", "banana", [",", "lemon"]]])
  """
  |> assert_eval(:ok)

  """
  IO.puts(["apple", ?,, "banana", ?,, "lemon"])
  """
  |> assert_eval(:ok)

  """
  IO.puts([?O, ?l, ?á, ?\\s, "Mary", ?!])
  """
  |> assert_eval(:ok)

  ## =======================================================================================================================
  ## alias, require, import, and use ======================================================================================
  ## =======================================================================================================================

  #  todo 26 using alias inside a module does not work
  #  """
  #  defmodule Stats do
  #    alias Math.List, as: List
  #    # In the remaining module definition List expands to Math.List.
  #  end
  #  """
  #  |> assert_eval_module()

  """
  alias Math.List
  """
  |> assert_eval(Math.List)

  """
  alias Math.List, as: List
  """
  |> assert_eval(Math.List)

  """
  Integer.is_odd(3)
  """
  |> assert_error(%UndefinedFunctionError{})

  """
  require Integer
  Integer.is_odd(3)
  """
  |> assert_eval(true)

  """
  import List, only: [duplicate: 2]
  duplicate(:ok, 3)
  """
  |> assert_eval([:ok, :ok, :ok])

  """
  defmodule Math do
    def some_function do
      import List, only: [duplicate: 2]
      duplicate(:ok, 10)
    end
  end
  """
  |> assert_eval_module()

  #  todo 27 Unable to open <ModuleName>.beam Failed load module: <ModuleName>.beam
  #  """
  #  defmodule AssertionTest do
  #    use ExUnit.Case, async: true
  #
  #    test "always pass" do
  #      assert true
  #    end
  #  end
  #  """
  #  |> assert_eval_module()

  #  todo 27 Unable to open <ModuleName>.beam Failed load module: <ModuleName>.beam
  #  """
  #  defmodule Example do
  #    use Feature, option: :value
  #  end
  #  """
  #  |> assert_eval_module()

  #  todo 27 Unable to open <ModuleName>.beam Failed load module: <ModuleName>.beam
  #  """
  #  defmodule Example do
  #    require Feature
  #    Feature.__using__(option: :value)
  #  end
  #  """
  #  |> assert_eval_module()

  """
  is_atom(String)
  """
  |> assert_eval(true)

  """
  to_string(String)
  """
  |> assert_eval("Elixir.String")

  """
  :"Elixir.String" == String
  """
  |> assert_eval(true)

  """
  List.flatten([1, [2], 3])
  """
  |> assert_eval([1, 2, 3])

  """
  :"Elixir.List".flatten([1, [2], 3])
  """
  |> assert_eval([1, 2, 3])

  """
  :lists.flatten([1, [2], 3])
  """
  |> assert_eval([1, 2, 3])

  #  todo 28 nested modules do not work
  #  """
  #  defmodule Foo do
  #    defmodule Bar do
  #    end
  #  end
  #  """
  #  |> assert_eval_module()

  #  """
  #  defmodule Foo.Bar do
  #  end
  #
  #  defmodule Foo do
  #    alias Foo.Bar
  #    # Can still access it as `Bar`
  #  end
  #  """
  #  |> assert_eval_module()

  #  """
  #  defmodule Foo do
  #    defmodule Bar do
  #      defmodule Baz do
  #      end
  #    end
  #  end
  #
  #  alias Foo.Bar.Baz
  #  # The module `Foo.Bar.Baz` is now available as `Baz`
  #  # However, the module `Foo.Bar` is *not* available as `Bar`
  #  """
  #  |> assert_eval(Foo.Bar.Baz)

  #  """
  #  alias MyApp.{Foo, Bar, Baz}
  #  """
  #  |> assert_eval(MyApp.{Foo, Bar, Baz})

  ## =======================================================================================================================
  ## Module attributes ====================================================================================================
  ## =======================================================================================================================

  """
  defmodule MyServer do
    @moduledoc "My server code."
  end
  """
  |> assert_eval_module()

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
  """
  |> assert_eval_module()

  #  todo 28 module attributes do not work
  #  """
  #  defmodule MyServer do
  #    @service URI.parse("https://example.com")
  #    IO.inspect(@service)
  #  end
  #  """
  #  |> assert_eval_module()
  #
  #  """
  #  defmodule MyServer do
  #    @unknown
  #  end
  #  """
  #  |> assert_eval_module()
  #
  #  """
  #  defmodule MyApp.Status do
  #    @service URI.parse("https://example.com")
  #    def status(email) do
  #      SomeHttpClient.get(@service)
  #    end
  #  end
  #  """
  #  |> assert_eval_module()
  #
  #  """
  #  defmodule MyApp.Status do
  #    def status(email) do
  #      SomeHttpClient.get(%URI{
  #        authority: "example.com",
  #        host: "example.com",
  #        port: 443,
  #        scheme: "https"
  #      })
  #    end
  #  end
  #  """
  #  |> assert_eval_module()

  # =======================================================================================================================
  # Structs ==============================================================================================================
  # =======================================================================================================================

  """
  map = %{a: 1, b: 2}
  map[:a]
  """
  |> assert_eval(1)

  """
  map = %{a: 1, b: 2}
  %{map | a: 3}
  """
  |> assert_eval(%{a: 3, b: 2})

  #  todo 29 implement/fix structs
  #    """
  #    defmodule User do
  #      defstruct name: "John", age: 27
  #    end
  #    """
  #    |> assert_eval_module()
  #
  #  """
  #  %User{}
  #  %User{age: 27, name: "John"}
  #  %User{name: "Jane"}
  #  %User{age: 27, name: "Jane"}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  %User{oops: :field}
  #  ** (KeyError) key :oops not found expanding struct: User.__struct__/1
  #  """
  #  |> assert_eval()
  #
  #  """
  #  john = %User{}
  #  %User{age: 27, name: "John"}
  #  john.name
  #  "John"
  #  jane = %{john | name: "Jane"}
  #  %User{age: 27, name: "Jane"}
  #  %{jane | oops: :field}
  #  ** (KeyError) key :oops not found in: %User{age: 27, name: "Jane"}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  %User{name: name} = john
  #  %User{age: 27, name: "John"}
  #  name
  #  "John"
  #  %User{} = %{}
  #  ** (MatchError) no match of right hand side value: %{}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  is_map(john)
  #  true
  #  john.__struct__
  #  User
  #  """
  #  |> assert_eval()
  
  #  @additional_prep """
  #  john = %User{}
  #  """
    
  #  """
  #  john[:name]
  #  """
  #  |> assert_eval(%UndefinedFunctionError{})
  #
  #  """
  #  Enum.each(john, fn {field, value} -> IO.puts(value) end)
  #  """
  #  |> assert_eval(%Protocol.UndefinedError{})

  #  todo 29 implement/fix structs
  #  """
  #  defmodule Product do
  #    defstruct [:name]
  #  end
  #  %Product{}
  #  """
  #  |> assert_eval(%{name: nil})
  #
  #  """
  #  defmodule User do
  #    defstruct [:email, name: "John", age: 27]
  #  end
  #  %User{}
  #  """
  #  |> assert_eval(%{age: 27, email: nil, name: "John"})

  #  """
  #  defmodule User do
  #    defstruct [name: "John", age: 27, :email]
  #  end
  #  """
  #  |> assert_error(%SyntaxError{})
  #
  #  """
  #  defmodule Car do
  #    @enforce_keys [:make]
  #    defstruct [:model, :make]
  #  end
  #  %Car{}
  #  """
  #  |> assert_error(%ArgumentError{})

  # =======================================================================================================================
  # Protocols ============================================================================================================
  # =======================================================================================================================

  """
  defmodule Utility do
    def type(value) when is_binary(value), do: "string"
    def type(value) when is_integer(value), do: "integer"
    # ... other implementations ...
  end
  """
  |> assert_eval_module()

  #  todo 30 implement/fix protocols
  #  """
  #  defprotocol Utility do
  #    @spec type(t) :: String.t()
  #    def type(value)
  #  end
  #
  #  defimpl Utility, for: BitString do
  #    def type(_value), do: "string"
  #  end
  #
  #  defimpl Utility, for: Integer do
  #    def type(_value), do: "integer"
  #  end
  #  
  #  Utility.type("foo")
  #  Utility.type(123)
  #  """
  #  |> assert_eval("integer")

  #
  #  """
  #  defprotocol Size do
  #    @doc "Calculates the size (and not the length!) of a data structure"
  #    def size(data)
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defimpl Size, for: BitString do
  #    def size(string), do: byte_size(string)
  #  end
  #
  #  defimpl Size, for: Map do
  #    def size(map), do: map_size(map)
  #  end
  #
  #  defimpl Size, for: Tuple do
  #    def size(tuple), do: tuple_size(tuple)
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  Size.size("foo")
  #  3
  #  Size.size({:ok, "hello"})
  #  2
  #  Size.size(%{label: "some label"})
  #  1
  #  """
  #  |> assert_eval()
  #
  #  """
  #  Size.size([1, 2, 3])
  #  ** (Protocol.UndefinedError) protocol Size not implemented for [1, 2, 3] of type List
  #  """
  #  |> assert_eval()
  #
  #  """
  #  Size.size(%{})
  #  0
  #  set = %MapSet{} = MapSet.new
  #  MapSet.new([])
  #  Size.size(set)
  #  ** (Protocol.UndefinedError) protocol Size not implemented for MapSet.new([]) of type MapSet (a struct)
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defimpl Size, for: MapSet do
  #    def size(set), do: MapSet.size(set)
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defmodule User do
  #    defstruct [:name, :age]
  #  end
  #
  #  defimpl Size, for: User do
  #    def size(_user), do: 2
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defimpl Size, for: Any do
  #    def size(_), do: 0
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defmodule OtherUser do
  #    @derive [Size]
  #    defstruct [:name, :age]
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defprotocol Size do
  #    @fallback_to_any true
  #    def size(data)
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  defimpl Size, for: Any do
  #    def size(_), do: 0
  #  end
  #  """
  #  |> assert_eval()
  #
  """
  Enum.reduce(1..3, 0, fn x, acc -> x + acc end)
  """
  |> assert_eval(6)

  """
  "age: \#{25}"
  """
  |> assert_eval("age: 25")

  """
  tuple = {1, 2, 3}
  "tuple: \#{tuple}"
  """
  |> assert_error(%Protocol.UndefinedError{})

  """
  tuple = {1, 2, 3}
  "tuple: \#{inspect(tuple)}"
  """
  |> assert_eval("tuple: {1, 2, 3}")
  
  #
  #  """
  #  {1, 2, 3}
  #  {1, 2, 3}
  #  %User{}
  #  %User{name: "john", age: 27}
  #  """
  #  |> assert_eval()
  #
  #  """
  #  inspect &(&1+2)
  #  "#Function<6.71889879/1 in :erl_eval.expr/5>"
  #  """
  #  |> assert_eval()
  #
  ## =======================================================================================================================
  ## Comprehensions =======================================================================================================
  ## =======================================================================================================================

  """
  for n <- [1, 2, 3, 4], do: n * n
  """
  |> assert_eval([1, 4, 9, 16])

  """
  for n <- 1..4, do: n * n
  """
  |> assert_eval([1, 4, 9, 16])

  """
  values = [good: 1, good: 2, bad: 3, good: 4]
  for {:good, n} <- values, do: n * n
  """
  |> assert_eval([1, 4, 16])

  """
  for n <- 0..5, rem(n, 3) == 0, do: n * n
  """
  |> assert_eval([0, 9])

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  dirs = ["/home/mikey", "/home/james"]
  #
  #  for dir <- dirs,
  #      file <- File.ls!(dir),
  #      path = Path.join(dir, file),
  #      File.regular?(path) do
  #    File.stat!(path).size
  #  end
  #  """
  #  |> assert_eval()

  """
  for i <- [:a, :b, :c], j <- [1, 2], do:  {i, j}
  """
  |> assert_eval(a: 1, a: 2, b: 1, b: 2, c: 1, c: 2)

  """
  pixels = <<213, 45, 132, 64, 76, 32, 76, 0, 0, 234, 32, 15>>
  for <<r::8, g::8, b::8 <- pixels>>, do: {r, g, b}
  """
  |> assert_eval([{213, 45, 132}, {64, 76, 32}, {76, 0, 0}, {234, 32, 15}])

  """
  for <<c <- " hello world ">>, c != ?\\s, into: "", do: <<c>>
  """
  |> assert_eval("helloworld")

  """
  for {key, val} <- %{"a" => 1, "b" => 2}, into: %{}, do: {key, val * val}
  """
  |> assert_eval(%{"a" => 1, "b" => 4})

  #  todo 38 testing IO - take result from try_run and output and assert it
  #  """
  #  stream = IO.stream(:stdio, :line)
  #  for line <- stream, into: stream do
  #    String.upcase(line) <> "\n"
  #  end
  #  """
  #  |> assert_eval()

  # =======================================================================================================================
  # Sigils ============================================================================================================
  # =======================================================================================================================

  #  todo 31 "~r" and "~i" do not work
  #  """
  #  # A regular expression that matches strings which contain "foo" or "bar":
  #  regex = ~r/foo|bar/
  #  "foo" =~ ~r/foo|bar/
  #  """
  #  |> assert_eval(true)
  #
  #  """
  #  # A regular expression that matches strings which contain "foo" or "bar":
  #  regex = ~r/foo|bar/
  #  "bat" =~ regex
  #  """
  #  |> assert_eval(false)

  #  """
  #  "HELLO" =~ ~r/hello/
  #  """
  #  |> assert_eval(false)
  #
  #  """
  #  "HELLO" =~ ~r/hello/i
  #  """
  #  |> assert_eval(true)
  #
  #  """
  #  ~r/hello/
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r|hello|
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r"hello"
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r'hello'
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r(hello)
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r[hello]
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r{hello}
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~r<hello>
  #  """
  #  |> assert_eval(~r/hello/)
  #
  #  """
  #  ~s(this is a string with "double" quotes, not 'single' ones)
  #  """
  #  |> assert_eval("this is a string with \"double\" quotes, not 'single' ones")
  #
  #  """
  #  [?c, ?a, ?t]
  #  """
  #  |> assert_eval(~c"cat")
  #
  #  """
  #  ~c(this is a char list containing "double quotes")
  #  """
  #  |> assert_eval(~c"this is a char list containing \"double quotes\"")
  #
  #  """
  #  ~w(foo bar bat)
  #  """
  #  |> assert_eval(["foo", "bar", "bat"])
  #
  #  """
  #  ~w(foo bar bat)a
  #  """
  #  |> assert_eval([:foo, :bar, :bat])

  """
  ~s(String with escape codes \x26 \#{"inter" <> "polation"})
  """
  |> assert_eval("String with escape codes & interpolation")

  """
  ~S(String without escape codes \\x26 without \#{interpolation})
  """
  |> assert_eval("String without escape codes \\x26 without \#{interpolation}")

  #  """
  #  ~s"""
  #  this is
  #  a heredoc string
  #  \"""
  #  """
  #  |> assert_eval()
  #
  #  """
  #  @doc """
  #  Converts double-quotes to single-quotes.
  #
  #  ## Examples
  #
  #      convert("\\\"foo\\\"")
  #      "'foo'"
  #
  #  \"""
  #  def convert(...)
  #  """
  #  |> assert_eval()
  #
  #  """
  #  @doc ~S"""
  #  Converts double-quotes to single-quotes.
  #
  #  ## Examples
  #
  #      convert("\"foo\"")
  #      "'foo'"
  #
  #  \"""
  #  def convert(...)
  #  """
  #  |> assert_eval()

  #  
  """
  d = ~D[2019-10-31]
  d.day
  """
  |> assert_eval(31)

  """
  t = ~T[23:00:07.0]
  t.second
  """
  |> assert_eval(7)

  """
  ndt = ~N[2019-10-31 23:00:07]
  """
  |> assert_eval(~N[2019-10-31 23:00:07])

  """
  dt = ~U[2019-10-31 19:59:03Z]
  %DateTime{minute: minute, time_zone: time_zone} = dt
  minute
  """
  |> assert_eval(59)

  """
  dt = ~U[2019-10-31 19:59:03Z]
  %DateTime{minute: minute, time_zone: time_zone} = dt
  time_zone
  """
  |> assert_eval("Etc/UTC")

  #  todo 31 "~r" and "~i" do not work
  #  """
  #  sigil_r(<<"foo">>, [?i])
  #  """
  #  |> assert_eval(~r"foo"i)

  #  todo 31 "~r" and "~i" do not work
  #  """
  #  defmodule MySigils do
  #    def sigil_i(string, []), do: String.to_integer(string)
  #    def sigil_i(string, [?n]), do: -String.to_integer(string)
  #  end
  #  import MySigils
  #  ~i(13)
  #  """
  #  |> assert_eval(13)
  #
  #  """
  #  defmodule MySigils do
  #    def sigil_i(string, []), do: String.to_integer(string)
  #    def sigil_i(string, [?n]), do: -String.to_integer(string)
  #  end
  #  import MySigils
  #  ~i(42)n
  #  """
  #  |> assert_eval(-42)

  # =======================================================================================================================
  # try, catch, and rescue ===============================================================================================
  # =======================================================================================================================

    """
    :foo + 1
    """
    |> assert_error(%ArithmeticError{})

    """
    raise "oops"
    """
    |> assert_error(%RuntimeError{message: "oops"})

    """
    raise ArgumentError, message: "invalid argument foo"
    """
    |> assert_error(%ArgumentError{})

  #  """
  #  defmodule MyError do
  #    defexception message: "default message"
  #  end
  #  raise MyError
  #  ** (MyError) default message
  #  raise MyError, message: "custom message"
  #  ** (MyError) custom message
  #  """
  #  |> assert_eval()

  """
  try do
    raise "oops"
  rescue
    e in RuntimeError -> e
  end
  """
  |> assert_eval(%RuntimeError{message: "oops"})

  """
  try do
    raise "oops"
  rescue
    RuntimeError -> "Error!"
  end
  """
  |> assert_eval("Error!")

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  File.read("hello")
  #  {:error, :enoent}
  #  File.write("hello", "world")
  #  :ok
  #  File.read("hello")
  #  {:ok, "world"}
  #  """
  #  |> assert_eval()

  #  """
  #  case File.read("hello") do
  #    {:ok, body} -> IO.puts("Success: #{body}")
  #    {:error, reason} -> IO.puts("Error: #{reason}")
  #  end
  #  """
  #  |> assert_eval()
  #
  #  """
  #  File.read!("unknown")
  #  ** (File.Error) could not read file "unknown": no such file or directory
  #      (elixir) lib/file.ex:272: File.read!/1
  #  """
  #  |> assert_eval()

  #  """
  #  try do
  #    ... some code ...
  #  rescue
  #    e ->
  #      Logger.error(Exception.format(:error, e, __STACKTRACE__))
  #      reraise e, __STACKTRACE__
  #  end
  #  """
  #  |> assert_eval()

  """
  try do
    Enum.each(-50..50, fn x ->
      if rem(x, 13) == 0, do: throw(x)
    end)
    "Got nothing"
  catch
    x -> "Got \#{x}"
  end
  """
  |> assert_eval("Got -39")

  """
  Enum.find(-50..50, &(rem(&1, 13) == 0))
  """
  |> assert_eval(-39)

  #  """
  #  spawn_link(fn -> exit(1) end)
  #  ** (EXIT from #PID<0.56.0>) shell process exited with reason: 1
  #  """
  #  |> assert_eval()

  """
  try do
    exit("I am exiting")
  catch
    :exit, _ -> "not really"
  end
  """
  |> assert_eval("not really")

  """
  defmodule Example do
    def matched_catch do
      exit(:timeout)
    catch
      :exit, :timeout ->
        {:error, :timeout}
    end

    def mismatched_catch do
      exit(:timeout)
    catch
      # Since no clause matches, this catch will have no effect
      :exit, :explosion ->
        {:error, :explosion}
    end
  end
  """
  |> assert_eval_module()

  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  {:ok, file} = File.open("sample", [:utf8, :write])
  #  try do
  #    IO.write(file, "olá")
  #    raise "oops, something went wrong"
  #  after
  #    File.close(file)
  #  end
  #  ** (RuntimeError) oops, something went wrong
  #  """
  #  |> assert_eval()

  #  """
  #  defmodule RunAfter do
  #    def without_even_trying do
  #      raise "oops"
  #    after
  #      IO.puts("cleaning up!")
  #    end
  #  end
  #  RunAfter.without_even_trying
  #  cleaning up!
  #  ** (RuntimeError) oops
  #  """
  #  |> assert_eval()

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
  """
  |> assert_eval(:small)

  """
  what_happened =
    try do
      raise "fail"
      :did_not_raise
    rescue
      _ -> :rescued
    end
  what_happened
  """
  |> assert_eval(:rescued)

  # =======================================================================================================================
  # Writing documentation ================================================================================================
  # =======================================================================================================================

  """
  defmodule MyApp.Sample do
    @doc false
    def add(a, b), do: a + b
  end
  """
  |> assert_eval_module()

  ## =======================================================================================================================
  ## Optional syntax sheet ================================================================================================
  ## =======================================================================================================================

  """
  length([1, 2, 3]) == length [1, 2, 3]
  """
  |> assert_eval(true)

  """
  # do-end blocks
  if true do
    :this
  else
    :that
  end
  """
  |> assert_eval(:this)

  """
  # keyword lists
  if true, do: :this, else: :that
  """
  |> assert_eval(:this)

  """
  defmodule(Math, [
    {:do, def(add(a, b), [{:do, a + b}])}
  ])
  """
  |> assert_eval_module()

  # =======================================================================================================================
  # Erlang libraries =====================================================================================================
  # =======================================================================================================================

  """
  is_atom(String)
  """
  |> assert_eval(true)

  """
  String.first("hello")
  """
  |> assert_eval("h")

  """
  is_atom(:binary)
  """
  |> assert_eval(true)

  """
  :binary.first("hello")
  """
  |> assert_eval(104)

  """
  String.to_charlist("Ø")
  """
  |> assert_eval([216])

  """
  :binary.bin_to_list("Ø")
  """
  |> assert_eval([195, 152])

  #  """
  #  :io.format("Pi is approximately given by:~10.3f~n", [:math.pi])
  #  """
  #  |> assert_eval(:ok)

  """
  to_string(:io_lib.format("Pi is approximately given by:~10.3f~n", [:math.pi]))
  """
  |> assert_eval("Pi is approximately given by:     3.142\n")

  """
  Base.encode16(:crypto.hash(:sha256, "Elixir"))
  """
  |> assert_eval("3315715A7A3AD57428298676C5AE465DADA38D951BDFAC9348A8A31E9C7401CB")

  #  todo 32 fix :digraph module
  #  """
  #  digraph = :digraph.new()
  #  coords = [{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}]
  #  [v0, v1, v2] = (for c <- coords, do: :digraph.add_vertex(digraph, c))
  #  :digraph.add_edge(digraph, v0, v1)
  #  :digraph.add_edge(digraph, v1, v2)
  #  :digraph.get_short_path(digraph, v0, v2)
  #  """
  #  |> assert_eval([{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}])

  #  todo 33 fix :ets module
  #  """
  #  table = :ets.new(:ets_test, [])
  #  # Store as tuples with {name, population}
  #  :ets.insert(table, {"China", 1_374_000_000})
  #  :ets.insert(table, {"India", 1_284_000_000})
  #  :ets.insert(table, {"USA", 322_000_000})
  #  :ets.tab2list(table)
  #  """
  #  |> assert_eval([])

  """
  angle_45_deg = :math.pi() * 45.0 / 180.0
  :math.sin(angle_45_deg)
  """
  |> assert_eval(0.7071067811865475)

  """
  :math.exp(55.0)
  """
  |> assert_eval(7.694785265142018e23)

  """
  :math.log(7.694785265142018e23)
  """
  |> assert_eval(55.0)

  """
  q = :queue.new
  q = :queue.in("A", q)
  q = :queue.in("B", q)
  {value, q} = :queue.out(q)
  value
  """
  |> assert_eval({:value, "A"})

  """
  q = :queue.new
  q = :queue.in("A", q)
  q = :queue.in("B", q)
  {value, q} = :queue.out(q)
  {value, q} = :queue.out(q)
  value
  """
  |> assert_eval({:value, "B"})

  """
  q = :queue.new
  q = :queue.in("A", q)
  q = :queue.in("B", q)
  {value, q} = :queue.out(q)
  {value, q} = :queue.out(q)
  {value, q} = :queue.out(q)
  value
  """
  |> assert_eval(:empty)

  #  todo 34 fix :rand module
  #  """
  #  :rand.uniform()
  #  """
  #  |> assert_eval({:expect_fn, &is_float/1})
  #
  #  """
  #  _ = :rand.seed(:exs1024, {123, 123534, 345345})
  #  :rand.uniform()
  #  """
  #  |> assert_eval({:expect_fn, &is_float/1})

  #  """
  #  :rand.uniform(6)
  #  6
  #  """
  #  |> assert_eval(6)
  #
  #  todo 37 module File - decide whether we want to implement an exemplary "path" to a file
  #  """
  #  :zip.foldl(fn _, _, _, acc -> acc + 1 end, 0, :binary.bin_to_list("file.zip"))
  #  """
  #  |> assert_eval({:ok, 633})

  """
  song = "
  Mary had a little lamb,
  His fleece was white as snow,
  And everywhere that Mary went,
  The lamb was sure to go."
  compressed = :zlib.compress(song)
  byte_size(song)
  """
  |> assert_eval(110)

  """
  song = "
  Mary had a little lamb,
  His fleece was white as snow,
  And everywhere that Mary went,
  The lamb was sure to go."
  compressed = :zlib.compress(song)
  byte_size(compressed)
  """
  |> assert_eval(99)

  #  todo 35 fix :zlib.uncompress/1
  #  """
  #  song = "
  #  Mary had a little lamb,
  #  His fleece was white as snow,
  #  And everywhere that Mary went,
  #  The lamb was sure to go."
  #  compressed = :zlib.compress(song)
  #  :zlib.uncompress(compressed)
  #  """
  #  |> assert_eval("\nMary had a little lamb,\nHis fleece was white as snow,\nAnd everywhere that Mary went,\nThe lamb was sure to go.")

  # =======================================================================================================================
  # Debugging ============================================================================================================
  # =======================================================================================================================

  #  todo 38 testing IO - take result from try_run and output and assert it
  #  """
  #  (1..10)
  #  |> IO.inspect()
  #  |> Enum.map(fn x -> x * 2 end)
  #  |> IO.inspect()
  #  |> Enum.sum()
  #  |> IO.inspect()
  #  """
  #  |> assert_eval()
  #
  ##  prints:
  ##
  ##  1..10
  ##  [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
  ##  110

  #  """
  #  [1, 2, 3]
  #  |> IO.inspect(label: "before")
  #  |> Enum.map(&(&1 * 2))
  #  |> IO.inspect(label: "after")
  #  |> Enum.sum
  #  """
  #  |> assert_eval()
  #
  ##  prints:
  ##
  ##  before: [1, 2, 3]
  ##  after: [2, 4, 6]

  # =======================================================================================================================
  # Enum cheatsheet =======================================================================================================
  # =======================================================================================================================

  @additional_prep """
  cart = [
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ]
  """

  """
  Enum.any?(cart, & &1.fruit == "orange")
  """
  |> assert_eval(true)

  """
  Enum.any?(cart, & &1.fruit == "pear")
  """
  |> assert_eval(false)

  """
  Enum.any?([], & &1.fruit == "orange")
  """
  |> assert_eval(false)

  """
  Enum.all?(cart, & &1.count > 0)
  """
  |> assert_eval(true)

  """
  Enum.all?(cart, & &1.count > 1)
  """
  |> assert_eval(false)

  """
  Enum.all?([], & &1.count > 0)
  """
  |> assert_eval(true)

  """
  Enum.member?(cart, %{fruit: "apple", count: 3})
  """
  |> assert_eval(true)

  """
  Enum.member?(cart, :something_else)
  """
  |> assert_eval(false)

  """
  %{fruit: "apple", count: 3} in cart
  """
  |> assert_eval(true)

  """
  :something_else in cart
  """
  |> assert_eval(false)

  """
  Enum.empty?(cart)
  """
  |> assert_eval(false)

  """
  Enum.empty?([])
  """
  |> assert_eval(true)

  #  todo 36 Fix "=~"
  #  """
  #  Enum.filter(cart, &(&1.fruit =~ "o"))
  #  """
  #  |> assert_eval([%{fruit: "orange", count: 6}])

  #  """
  #  Enum.filter(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval([
  #    %{fruit: "apple", count: 3},
  #    %{fruit: "orange", count: 6}
  #  ])

  #  """
  #  Enum.reject(cart, &(&1.fruit =~ "o"))
  #  """
  #  |> assert_eval([
  #        %{fruit: "apple", count: 3},
  #        %{fruit: "banana", count: 1}
  #      ])

  """
  Enum.flat_map(cart, fn item ->
    if item.count > 1, do: [item.fruit], else: []
  end)
  """
  |> assert_eval(["apple", "orange"])

  #  """
  #  for item <- cart, item.fruit =~ "e" do
  #     item
  #   end
  #  """
  #  |> assert_eval([
  #        %{fruit: "apple", count: 3},
  #        %{fruit: "orange", count: 6}
  #      ])

  """
  for %{count: 1, fruit: fruit} <- cart do
     fruit
   end
  """
  |> assert_eval(["banana"])

  """
  Enum.map(cart, & &1.fruit)
  ["apple", "banana", "orange"]
  Enum.map(cart, fn item ->
     %{item | count: item.count + 10}
   end)
  """
  |> assert_eval([
    %{fruit: "apple", count: 13},
    %{fruit: "banana", count: 11},
    %{fruit: "orange", count: 16}
  ])

  """
  Enum.map_every(cart, 2, fn item ->
     %{item | count: item.count + 10}
   end)
  """
  |> assert_eval([
    %{fruit: "apple", count: 13},
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 16}
  ])

  """
  for item <- cart do
     item.fruit
   end
  """
  |> assert_eval(["apple", "banana", "orange"])

  #  """
  #  for item <- cart, item.fruit =~ "e" do
  #     item.fruit
  #   end
  #  """
  #  |> assert_eval(["apple", "orange"])

  #  todo 38 testing IO - take result from try_run and output and assert it
  #  """
  #  Enum.each(cart, &IO.puts(&1.fruit))
  #  apple
  #  banana
  #  orange
  #  :ok
  #  """
  #  |> assert_eval()

  """
  Enum.reduce(cart, 0, fn item, acc ->
     item.count + acc
   end)
  """
  |> assert_eval(10)

  """
  Enum.map_reduce(cart, 0, fn item, acc ->
     {item.fruit, item.count + acc}
   end)
  """
  |> assert_eval({["apple", "banana", "orange"], 10})

  """
  Enum.scan(cart, 0, fn item, acc ->
     item.count + acc
   end)
  """
  |> assert_eval([3, 4, 10])

  """
  Enum.reduce_while(cart, 0, fn item, acc ->
     if item.fruit == "orange" do
       {:halt, acc}
     else
       {:cont, item.count + acc}
     end
   end)
  """
  |> assert_eval(4)

  """
  for item <- cart, reduce: 0 do
     acc -> item.count + acc
   end
  """
  |> assert_eval(10)

  #  """
  #  for item <- cart, item.fruit =~ "e", reduce: 0 do
  #     acc -> item.count + acc
  #   end
  #  """
  #  |> assert_eval(9)

  """
  Enum.count(cart)
  """
  |> assert_eval(3)

  """
  Enum.frequencies(["apple", "banana", "orange", "apple"])
  """
  |> assert_eval(%{"apple" => 2, "banana" => 1, "orange" => 1})

  """
  Enum.frequencies_by(cart, &String.last(&1.fruit))
  """
  |> assert_eval(%{"a" => 1, "e" => 2})

  #  """
  #  Enum.count(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval(2)

  #  """
  #  Enum.count(cart, &(&1.fruit =~ "y"))
  #  """
  #  |> assert_eval(0)

  """
  cart |> Enum.map(& &1.count) |> Enum.sum()
  """
  |> assert_eval(10)

  #  """
  #  Enum.sum_by(cart, & &1.count)
  #  """
  #  |> assert_eval(10)

  """
  cart |> Enum.map(& &1.count) |> Enum.product()
  """
  |> assert_eval(18)

  #  """
  #  Enum.product_by(cart, & &1.count)
  #  """
  #  |> assert_eval(18)

  """
  cart |> Enum.map(& &1.fruit) |> Enum.sort()
  """
  |> assert_eval(["apple", "banana", "orange"])

  """
  cart |> Enum.map(& &1.fruit) |> Enum.sort(:desc)
  """
  |> assert_eval(["orange", "banana", "apple"])

  """
  Enum.sort_by(cart, & &1.count)
  """
  |> assert_eval([
    %{fruit: "banana", count: 1},
    %{fruit: "apple", count: 3},
    %{fruit: "orange", count: 6}
  ])

  """
  Enum.sort_by(cart, & &1.count, :desc)
  """
  |> assert_eval([
    %{fruit: "orange", count: 6},
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1}
  ])

  """
  cart |> Enum.map(& &1.count) |> Enum.min()
  """
  |> assert_eval(1)

  #  """
  #  Enum.min_by(cart, & &1.count)
  #  """
  #  |> assert_eval(%{fruit: "banana", count: 1})

  """
  cart |> Enum.map(& &1.count) |> Enum.max()
  """
  |> assert_eval(6)

  #  """
  #  Enum.max_by(cart, & &1.count)
  #  """
  #  |> assert_eval(%{fruit: "orange", count: 6})

  """
  Enum.concat([[1, 2, 3], [4, 5, 6], [7, 8, 9]])
  """
  |> assert_eval([1, 2, 3, 4, 5, 6, 7, 8, 9])

  """
  Enum.concat([1, 2, 3], [4, 5, 6])
  """
  |> assert_eval([1, 2, 3, 4, 5, 6])

  """
  Enum.flat_map(cart, fn item ->
     List.duplicate(item.fruit, item.count)
   end)
  """
  |> assert_eval([
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
  ])

  """
  Enum.flat_map_reduce(cart, 0, fn item, acc ->
     list = List.duplicate(item.fruit, item.count)
     acc = acc + item.count
     {list, acc}
   end)
  """
  |> assert_eval(
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
  )

  """
  for item <- cart,
       fruit <- List.duplicate(item.fruit, item.count) do
     fruit
   end
  """
  |> assert_eval([
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
  ])

  """
  pairs = [{"apple", 3}, {"banana", 1}, {"orange", 6}]
  Enum.into(pairs, %{})
  """
  |> assert_eval(%{"apple" => 3, "banana" => 1, "orange" => 6})

  """
  Enum.into(cart, %{}, fn item ->
     {item.fruit, item.count}
   end)
  """
  |> assert_eval(%{"apple" => 3, "banana" => 1, "orange" => 6})

  """
  Enum.to_list(1..5)
  """
  |> assert_eval([1, 2, 3, 4, 5])

  """
  for item <- cart, into: %{} do
     {item.fruit, item.count}
   end
  """
  |> assert_eval(%{"apple" => 3, "banana" => 1, "orange" => 6})

  """
  Enum.dedup([1, 2, 2, 3, 3, 3, 1, 2, 3])
  """
  |> assert_eval([1, 2, 3, 1, 2, 3])

  #  """
  #  Enum.dedup_by(cart, & &1.fruit =~ "a")
  #  """
  #  |> assert_eval([%{fruit: "apple", count: 3}])

  """
  Enum.dedup_by(cart, & &1.count < 5)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "orange", count: 6}
  ])

  """
  Enum.uniq([1, 2, 2, 3, 3, 3, 1, 2, 3])
  """
  |> assert_eval([1, 2, 3])

  """
  Enum.uniq_by(cart, &String.last(&1.fruit))
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1}
  ])

  """
  Enum.at(cart, 0)
  """
  |> assert_eval(%{fruit: "apple", count: 3})

  """
  Enum.at(cart, 10)
  """
  |> assert_eval(nil)

  """
  Enum.at(cart, 10, :none)
  """
  |> assert_eval(:none)

  """
  Enum.fetch(cart, 0)
  """
  |> assert_eval({:ok, %{fruit: "apple", count: 3}})

  """
  Enum.fetch(cart, 10)
  """
  |> assert_eval(:error)

  """
  Enum.fetch!(cart, 0)
  """
  |> assert_eval(%{fruit: "apple", count: 3})

  """
  Enum.fetch!(cart, 10)
  """
  |> assert_error(%Enum.OutOfBoundsError{})

  """
  Enum.with_index(cart)
  """
  |> assert_eval([
    {%{fruit: "apple", count: 3}, 0},
    {%{fruit: "banana", count: 1}, 1},
    {%{fruit: "orange", count: 6}, 2}
  ])

  """
  Enum.with_index(cart, fn item, index ->
     {item.fruit, index}
   end)
  """
  |> assert_eval([
    {"apple", 0},
    {"banana", 1},
    {"orange", 2}
  ])

  #  """
  #  Enum.find(cart, &(&1.fruit =~ "o"))
  #  """
  #  |> assert_eval(%{fruit: "orange", count: 6})

  #  """
  #  Enum.find(cart, &(&1.fruit =~ "y"))
  #  """
  #  |> assert_eval(nil)

  #  """
  #  Enum.find(cart, :none, &(&1.fruit =~ "y"))
  #  """
  #  |> assert_eval(:none)

  #  """
  #  Enum.find_index(cart, &(&1.fruit =~ "o"))
  #  """
  #  |> assert_eval(2)

  #  """
  #  Enum.find_index(cart, &(&1.fruit =~ "y"))
  #  """
  #  |> assert_eval(nil)

  """
  Enum.find_value(cart, fn item ->
     if item.count == 1, do: item.fruit, else: nil
   end)
  """
  |> assert_eval("banana")

  """
  Enum.find_value(cart, :none, fn item ->
     if item.count == 100, do: item.fruit, else: nil
   end)
  """
  |> assert_eval(:none)

  """
  Enum.group_by(cart, &String.last(&1.fruit))
  """
  |> assert_eval(%{
    "a" => [%{fruit: "banana", count: 1}],
    "e" => [
      %{fruit: "apple", count: 3},
      %{fruit: "orange", count: 6}
    ]
  })

  """
  Enum.group_by(cart, &String.last(&1.fruit), & &1.fruit)
  """
  |> assert_eval(%{
    "a" => ["banana"],
    "e" => ["apple", "orange"]
  })

  """
  Enum.join(["apple", "banana", "orange"], ", ")
  """
  |> assert_eval("apple, banana, orange")

  """
  Enum.map_join(cart, ", ", & &1.fruit)
  """
  |> assert_eval("apple, banana, orange")

  """
  Enum.intersperse(["apple", "banana", "orange"], ", ")
  """
  |> assert_eval(["apple", ", ", "banana", ", ", "orange"])

  """
  Enum.map_intersperse(cart, ", ", & &1.fruit)
  """
  |> assert_eval(["apple", ", ", "banana", ", ", "orange"])

  """
  Enum.slice(cart, 0..1)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1}
  ])

  """
  Enum.slice(cart, -2..-1)
  """
  |> assert_eval([
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ])

  """
  Enum.slice(cart, 1, 2)
  """
  |> assert_eval([
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ])

  @additional_prep """
  fruits = ["apple", "banana", "grape", "orange", "pear"]
  """

  #  """
  #  Enum.slide(fruits, 2, 0)
  #  """
  #  |> assert_eval(["grape", "apple", "banana", "orange", "pear"])

  #  """
  #  Enum.slide(fruits, 2, 4)
  #  """
  #  |> assert_eval(["apple", "banana", "orange", "pear", "grape"])

  """
  Enum.slide(fruits, 1..3, 0)
  """
  |> assert_eval(["banana", "grape", "orange", "apple", "pear"])

  """
  Enum.slide(fruits, 1..3, 4)
  """
  |> assert_eval(["apple", "pear", "banana", "grape", "orange"])

  @additional_prep """
  cart = [
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ]
  """

  """
  Enum.reverse(cart)
  """
  |> assert_eval([
    %{fruit: "orange", count: 6},
    %{fruit: "banana", count: 1},
    %{fruit: "apple", count: 3}
  ])

  """
  Enum.reverse(cart, [:this_will_be, :the_tail])
  """
  |> assert_eval([
    %{fruit: "orange", count: 6},
    %{fruit: "banana", count: 1},
    %{fruit: "apple", count: 3},
    :this_will_be,
    :the_tail
  ])

  """
  Enum.reverse_slice(cart, 1, 2)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "orange", count: 6},
    %{fruit: "banana", count: 1}
  ])

  """
  Enum.split(cart, 1)
  """
  |> assert_eval(
    {[%{fruit: "apple", count: 3}],
     [
       %{fruit: "banana", count: 1},
       %{fruit: "orange", count: 6}
     ]}
  )

  """
  Enum.split(cart, -1)
  """
  |> assert_eval(
    {[
       %{fruit: "apple", count: 3},
       %{fruit: "banana", count: 1}
     ], [%{fruit: "orange", count: 6}]}
  )

  #  """
  #  Enum.split_while(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval(
  #    {[%{fruit: "apple", count: 3}],
  #     [
  #       %{fruit: "banana", count: 1},
  #       %{fruit: "orange", count: 6}
  #     ]}
  #  )

  #  """
  #  Enum.split_with(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval(
  #    {[
  #       %{fruit: "apple", count: 3},
  #       %{fruit: "orange", count: 6}
  #     ], [%{fruit: "banana", count: 1}]}
  #  )

  """
  Enum.drop(cart, 1)
  """
  |> assert_eval([
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ])

  """
  Enum.drop(cart, -1)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1}
  ])

  """
  Enum.drop_every(cart, 2)
  """
  |> assert_eval([%{fruit: "banana", count: 1}])

  #  """
  #  Enum.drop_while(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval([
  #    %{fruit: "banana", count: 1},
  #    %{fruit: "orange", count: 6}
  #  ])

  """
  Enum.take(cart, 1)
  """
  |> assert_eval([%{fruit: "apple", count: 3}])

  """
  Enum.take(cart, -1)
  """
  |> assert_eval([%{fruit: "orange", count: 6}])

  """
  Enum.take_every(cart, 2)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "orange", count: 6}
  ])

  #  """
  #  Enum.take_while(cart, &(&1.fruit =~ "e"))
  #  """
  #  |> assert_eval([%{fruit: "apple", count: 3}])

  """
  Enum.chunk_by(cart, &String.length(&1.fruit))
  """
  |> assert_eval([
    [%{fruit: "apple", count: 3}],
    [
      %{fruit: "banana", count: 1},
      %{fruit: "orange", count: 6}
    ]
  ])

  """
  Enum.chunk_every(cart, 2)
  """
  |> assert_eval([
    [
      %{fruit: "apple", count: 3},
      %{fruit: "banana", count: 1}
    ],
    [%{fruit: "orange", count: 6}]
  ])

  """
  Enum.chunk_every(cart, 2, 2, [:elements, :to_complete])
  """
  |> assert_eval([
    [
      %{fruit: "apple", count: 3},
      %{fruit: "banana", count: 1}
    ],
    [
      %{fruit: "orange", count: 6},
      :elements
    ]
  ])

  """
  Enum.chunk_every(cart, 2, 1, :discard)
  """
  |> assert_eval([
    [
      %{fruit: "apple", count: 3},
      %{fruit: "banana", count: 1}
    ],
    [
      %{fruit: "banana", count: 1},
      %{fruit: "orange", count: 6}
    ]
  ])

  """
  fruits = ["apple", "banana", "orange"]
  counts = [3, 1, 6]
  Enum.zip(fruits, counts)
  """
  |> assert_eval([{"apple", 3}, {"banana", 1}, {"orange", 6}])

  """
  fruits = ["apple", "banana", "orange"]
  counts = [3, 1, 6]
  Enum.zip_with(fruits, counts, fn fruit, count ->
     %{fruit: fruit, count: count}
   end)
  """
  |> assert_eval([
    %{fruit: "apple", count: 3},
    %{fruit: "banana", count: 1},
    %{fruit: "orange", count: 6}
  ])

  #  """
  #  fruits = ["apple", "banana", "orange"]
  #  counts = [3, 1, 6]
  #  Enum.zip_reduce(fruits, counts, 0, fn fruit, count, acc ->
  #     price = if fruit =~ "e", do: count * 2, else: count
  #     acc + price
  #   end)
  #  """
  #  |> assert_eval(19)

  """
  cart |> Enum.map(&{&1.fruit, &1.count}) |> Enum.unzip()
  """
  |> assert_eval({["apple", "banana", "orange"], [3, 1, 6]})
end
