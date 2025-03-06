defmodule FissionLib.GettingStartedTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest

  @moduletag :tmp_dir

#=======================================================================================================================
# Introduction =========================================================================================================
#=======================================================================================================================
  
  assert_eval("40 + 2\n", 42)
  
  """
  "hello" <> " world"
  """
  |> assert_eval("hello world")
  
#=======================================================================================================================
# Basic types ==========================================================================================================
#=======================================================================================================================
  
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

  # todo assert_eval("1 and true\n", "** (BadBooleanError) expected a boolean on left-side of "and", got: 1")

  assert_eval("false and raise(\"This error will never be raised\")\n", false)
  assert_eval("true or raise(\"This error will never be raised\")\n", true)

  # todo not implemented '||'
  #    assert_eval("1 || true\n", 1)
  #    assert_eval("false || 11\n", 11)
  # todo not implemented '&&'
  #    assert_eval("nil && 13\n", nil)
  #    assert_eval("true && 17\n", 17)

  assert_eval("!true\n", false)
  #    todo '!' not implemented for integer or nil
  #    assert_eval("!1\n", false)
  #    assert_eval("!nil\n", true)
  assert_eval(":apple\n", :apple)
  assert_eval(":orange\n" ,:orange)
  assert_eval(":watermelon\n" ,:watermelon)

  assert_eval(":apple == :apple\n", true)
  assert_eval(":apple == :orange\n", false)

  assert_eval("true == :true\n", true)
  assert_eval("is_atom(false)\n", true)
  assert_eval("is_boolean(:false)\n", true)

  assert_eval("\"hellö\"", "hellö")

  assert_eval("\"hello \" <> \"world!\"", "hello world!")
  # todo #{} is not working
  #    """
  #    string = "world"
  #    "hello \#\{string\}!"
  #    """
  #    |> assert_eval("hello world!")
  
  #    """
  #    number = 42
  #    "i am \#\{number\} years old!"
  #    """
  #    |> assert_eval("i am 42 years old!")
  
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

#=======================================================================================================================
# Lists and tuples =====================================================================================================
#=======================================================================================================================
  
  assert_eval("[1, 2, true, 3]\n", [1, 2, true, 3])
  assert_eval("length([1, 2, 3])\n", 3)

  assert_eval("[1, 2, 3] ++ [4, 5, 6]\n", [1, 2, 3, 4, 5, 6])
#  todo assert_eval("[1, true, 2, false, 3, true] -- [true, false]\n", [1, 2, 3, true])

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

#    todo hd([]) "** (ArgumentError) argument error"

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

#  todo idk why String.split is failing
#  assert_eval("String.split(\"hello world\")\n", ["hello", "world"])
#  assert_eval("String.split(\"hello beautiful world\")\n", ["hello", "beautiful", "world"])

  assert_eval("String.split_at(\"hello world\", 3)", {"hel", "lo world"})
  assert_eval("String.split_at(\"hello world\", -4)", {"hello w", "orld"})

#  todo i dont think we are able to test it but idrk
#  assert_eval("File.read(\"path/to/existing/file\")", {:ok, "... contents ..."})
#  assert_eval("File.read(\"path/to/unknown/file\")", {:error, :enoent})
  
  """
  tuple = {:ok, "hello"}
  elem(tuple, 1)
  
  """
  |> assert_eval("hello")

#=======================================================================================================================
# Pattern matching =====================================================================================================
#=======================================================================================================================
  """
  x = 1
  x
  """
  |> assert_eval(1)
  
  # todo create a assert_badmatch function or macro
  #  """
  #  x = 1
  #  1 = x
  #  2 = x
  #  """
  #  |> assert_badmatch() todo create a assert_undefined_variable function or macro
  #  """
  #  1 = unknown
  #  """
  #  |> assert_undefined_variable()
  #
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

  #  """
  #  {a, b, c} = {:hello, "world"}
  #  """
  #  |> assert_badmatch()

  #  """
  #  {a, b, c} = [:hello, "world", 42]
  #  """
  #  |> assert_badmatch()

  """
  {:ok, result} = {:ok, 13}
  result
  """
  |> assert_eval(13)
  
  #  """
  #  {:ok, result} = {:ok, 13}
  #  {:ok, result} = {:error, :oops}
  #  """
  #  |> assert_badmatch()

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
  
  #  """
  #  [head | tail] = []
  #  """
  #  |> assert_badmatch()

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

  #  """
  #  x = 1
  #  ^x = 2
  #  """
  #  |> assert_badmatch()
  
  #  """
  #  1 = 2
  #  """
  #  |> assert_badmatch()
  
  #  """
  #  x = 1
  #  [^x, 2, 3] = [1, 2, 3]
  #  {y, ^x} = {2, 1}
  #  y
  #  {y, ^x} = {2, 2}
  #  """
  #  |> assert_badmatch()
  
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
  
  #  """
  #  {y, 1} = {2, 2}
  #  """
  #  |> assert_badmatch()
  
  """
  [head | _] = [1, 2, 3]
  head
  """
  |> assert_eval(1)

  #  """
  #  _
  #  """
  #  |> assert_compile_error()

  #  """
  #  length([1, [2], 3]) = 3
  #  """
  #  |> assert_compile_error()

#=======================================================================================================================
# case, cond, and if ===================================================================================================
#=======================================================================================================================

# todo AVM does not like cases with pattern matching to not used variables? "_x" works just fine xD
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

  #  todo
  #  """
  #  hd(1)
  #  """
  #  |> assert_argument_error()

#  """
#  case 1 do
#    x when hd(x) -> "Won't match"
#    x -> "Got \#\{x\}"
#  end
#  """
#  |> assert_eval("Got 1")
  
#  todo
#  """
#  case :ok do
#    :error -> "Won't match"
#  end
#  """
#  |> assert_case_clause_error()

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
#  todo if nil is not working as expected
#  """
#  if nil do
#    "This won't be seen"
#  else
#    "This will"
#  end
#  """
#  |> assert_eval(nil)
  
#  todo if true is not working as expected
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

#=======================================================================================================================
# Anonymous functions ==================================================================================================
#=======================================================================================================================

# todo think if it should display documentation string
  #  """
  #  h trunc/1
  #  """
  #  |> assert_eval("")
  #  """
  #  h Kernel.trunc/1
  #  """
  #  |> assert_eval("")

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

#  todo is_function/2 not implemented?
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

#  todo i guess running function like (fn x -> x end).(1) does not work
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

#   todo
#  """
#  f2 = fn
#  x, y when x > 0 -> x + y
#  x, y, z -> x * y + z
#  end
#  """
#  |> assert_compile_error()

  """
  fun = &is_atom/1
  """
  |> assert_eval(&:erlang.is_atom/1)

  """
  fun = &is_atom/1
  is_function(fun)
  
  """
  |> assert_eval(true)
  
#  todo guards as anonymous functions are not working (adding module to the function [Kernel. or :erlang.] does not help)
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

#  todo I guess this is similar to is_atom/1 situation
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

#  todo - known problem with #{}
#  """
#  fun2 = &"Good \#\{&1\}"
#  fun2.("morning")
#  """
#  |> assert_eval("Good morning")

#=======================================================================================================================
# Binaries, strings, and charlists =====================================================================================
#=======================================================================================================================

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

#  todo
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

#  todo
#  """
#  IO.inspect("hełło", binaries: :as_binaries)
#  """
#  |> assert_eval(<<104, 101, 197, 130, 197, 130, 111>>)

# todo "::" does not work for binaries
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
  
#  todo
#  """
#  <<0, 1, x>> = <<0, 1, 2, 3>>
#  """
#  |> assert_badmatch()

# todo
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
  
#  todo "'cat'" instead of "[99, 97, 116]"
#  """
#  heartbeats_per_minute = [99, 97, 116]
#  inspect(heartbeats_per_minute, charlists: :as_list)
#  """
#  |> assert_eval("[99, 97, 116]")
  
  """
  to_charlist("hełło")
  """
  |> assert_eval([104, 101, 322, 322, 111])

  #  todo "heBBo" instead of "hełło"
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
  
  #  todo
  #  """
  #  ~c"this " <> ~c"fails"
  #  """
  #  |> assert_argument_error()
  
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

#=======================================================================================================================
# Keyword lists and maps ===============================================================================================
#=======================================================================================================================

  """
  String.split("1 2 3 4", " ")
  """
  |> assert_eval(["1", "2", "3", "4"])

#  todo Keyword not works in option here??
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
  |> assert_eval([a: 1, b: 2, c: 3])


  """
  list = [a: 1, b: 2]
  [a: 0] ++ list
  """
  |> assert_eval([a: 0, a: 1, b: 2])


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

#  todo
#  """
#  [a: a] = [a: 1, b: 2]
#  ** (MatchError) no match of right hand side value: [a: 1, b: 2]
#  """
#  |> assert_error()
#
#
#  """
#  [b: b, a: a] = [a: 1, b: 2]
#  ** (MatchError) no match of right hand side value: [a: 1, b: 2]
#  """
#  |> assert_error()


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

#  todo
#  """
#  %{:c => c} = %{:a => 1, 2 => :b}
#  ** (MatchError) no match of right hand side value: %{2 => :b, :a => 1}
#  """
#  |> assert_error()


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


  """
  map = %{name: "John", age: 23}
  map.name
  """
  |> assert_eval("John")

#todo
#  """
#  map.agee
#  ** (KeyError) key :agee not found in: %{name: "John", age: 23}
#  """
#  |> assert_error()


  """
  map = %{name: "John", age: 23}
  %{map | name: "Mary"}
  """
  |> assert_eval(%{name: "Mary", age: 23})

# todo
#  """
#  %{map | agee: 27}
#  ** (KeyError) key :agee not found in: %{name: "John", age: 23}
#  """
#  |> assert_error()


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
       [
         john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
         mary: %{age: 29, languages: ["Elixir", "F#", "Clojure"], name: "Mary"}
       ]
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
       [
         john: %{age: 31, languages: ["Erlang", "Ruby", "Elixir"], name: "John"},
         mary: %{age: 29, languages: ["Elixir", "F#"], name: "Mary"}
       ]
     )
     
#=======================================================================================================================
# Modules and functions ================================================================================================
#=======================================================================================================================

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

#  todo
#  """
#  defmodule Math do
#    def sum(a, b) do
#      do_sum(a, b)
#    end
#
#    defp do_sum(a, b) do
#      a + b
#    end
#  end
#  Math.do_sum(1,2)
#  """
#  |> assert_error()

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
#  Math.zero?([1, 2, 3])
#  """
#  |> assert_function_clause_error()

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
#  |> assert_function_clause_error()

  """
  defmodule Math do
    def zero?(0), do: true
    def zero?(x) when is_integer(x), do: false
  end
  """
  |> assert_eval_module()

#  todo "<>" does not work for variables - works only for plain strings
#  todo default arguments defined with "\\" also have some problems
#  """
#  defmodule Concat do
#    def join(a, b, sep \\ " ") do
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

##=======================================================================================================================
## Recursion ============================================================================================================
##=======================================================================================================================

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

  Recursion.print_multiple_times("Hello!", 3)
  """
  |> assert_eval(:ok)

#  """
#  Recursion.print_multiple_times("Hello!", -1)
#  ** (FunctionClauseError) no function clause matching in Recursion.print_multiple_times/2
#
#      The following arguments were given to Recursion.print_multiple_times/2:
#
#          # 1
#          "Hello!"
#
#          # 2
#          -1
#
#      iex:1: Recursion.print_multiple_times/2
#  """
#  |> assert_function_clause_error()

  """
  defmodule Math do
    def sum_list([head | tail], accumulator) do
      sum_list(tail, head + accumulator)
    end

    def sum_list([], accumulator) do
      accumulator
    end
  end

  Math.sum_list([1, 2, 3], 0)
  """
  |> assert_eval(6)


  """
  defmodule Math do
    def sum_list([head | tail], accumulator) do
      sum_list(tail, head + accumulator)
    end

    def sum_list([], accumulator) do
      accumulator
    end
  end

  Math.sum_list([2, 3], 1)
  """
  |> assert_eval(6)


  """
  defmodule Math do
    def sum_list([head | tail], accumulator) do
      sum_list(tail, head + accumulator)
    end

    def sum_list([], accumulator) do
      accumulator
    end
  end

  Math.sum_list([3], 3)
  """
  |> assert_eval(6)


  """
  defmodule Math do
    def sum_list([head | tail], accumulator) do
      sum_list(tail, head + accumulator)
    end

    def sum_list([], accumulator) do
      accumulator
    end
  end

  Math.sum_list([], 6)
  """
  |> assert_eval(6)

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
  Enum.reduce([1, 2, 3], 0, fn x, acc -> x + acc end)
  """
  |> assert_eval(6)
  
  """
  Enum.map([1, 2, 3], &(&1 * 2))
  """
  |> assert_eval([2, 4, 6])
#  
##=======================================================================================================================
## Enumerables and Streams ==============================================================================================
##=======================================================================================================================

  """
  Enum.map(%{1 => 2, 3 => 4}, fn {k, v} -> k * v end)
  """
  |> assert_eval([2, 12])

  """
  Enum.map(1..3, fn x -> x * 2 end)
  """
  |> assert_eval([2, 4, 6])

#  """
#  Enum.reduce(1..3, 0, &+/2)
#  """
#  |> assert_eval(6)

  """
  odd? = fn x -> rem(x, 2) != 0 end
  Enum.filter(1..3, odd?)
  """
  |> assert_eval([1, 3])
  
#  todo the following freezes test execution idk why xDD
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

#  todo assert_is_stream?
#  """
#  1..100_000 |> Stream.map(&(&1 * 3))
#  #Stream<[enum: 1..100000, funs: [#Function<34.16982430/1 in Stream.map/2>]]>
#  """
#  |> assert_eval()
#
#  """
#  1..100_000 |> Stream.map(&(&1 * 3)) |> Stream.filter(odd?)
#  #Stream<[enum: 1..100000, funs: [...]]>
#  """
#  |> assert_eval()

  """
  stream = Stream.cycle([1, 2, 3])
  Enum.take(stream, 10)
  """
  |> assert_eval([1, 2, 3, 1, 2, 3, 1, 2, 3, 1])

#  todo File - idk if we want to test this module at all
#  """
#  "path/to/file" |> File.stream!() |> Enum.take(10)
#  """
#  |> assert_eval()

##=======================================================================================================================
## Processes ============================================================================================================
##=======================================================================================================================

# todo "unknown external type: 83"
#  """
#  spawn(fn -> 1 + 2 end)
#  """
#  |> assert_eval({:expect_fn, &is_pid/1})

#  todo process with "pid" is still alive (Process.alive?/1 is true)
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

#  todo 
#  """
#  spawn(fn -> raise "oops" end)
#  #PID<0.58.0>
#
#  [error] Process #PID<0.58.00> raised an exception
#  ** (RuntimeError) oops
#      (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
#  """
#  |> assert_error()

#  todo
#  """
#  self()
#  #PID<0.41.0>
#  spawn_link(fn -> raise "oops" end)
#
#  ** (EXIT from #PID<0.41.0>) evaluator process exited with reason: an exception was raised:
#      ** (RuntimeError) oops
#          (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
#
#  [error] Process #PID<0.289.0> raised an exception
#  ** (RuntimeError) oops
#      (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
#  """
#  |> assert_error()

#  todo
#  """
#  Task.start(fn -> raise "oops" end)
#  {:ok, #PID<0.55.0>}
#
#  15:22:33.046 [error] Task #PID<0.55.0> started from #PID<0.53.0> terminating
#  ** (RuntimeError) oops
#      (stdlib) erl_eval.erl:668: :erl_eval.do_apply/6
#      (elixir) lib/task/supervised.ex:85: Task.Supervised.do_apply/2
#      (stdlib) proc_lib.erl:247: :proc_lib.init_p_do_apply/3
#  Function: #Function<20.99386804/0 in :erl_eval.expr/5>
#      Args: []
#  """
#  |> assert_error()

#  todo
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

#  todo
#  """
#  {:ok, pid} = Agent.start_link(fn -> %{} end)
#  Agent.update(pid, fn map -> Map.put(map, :hello, :world) end)
#  Agent.get(pid, fn map -> Map.get(map, :hello) end)
#  """
#  |> assert_eval(:world)
  
#
##=======================================================================================================================
## IO and the file system ===============================================================================================
##=======================================================================================================================

#  todo idk how to test it
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

#  todo idk how to test it
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
#  """
#  name = "Mary"
#  IO.puts("Hello " <> name <> "!")
#  """
#  |> assert_eval()
#
#  """
#  name = "Mary"
#  IO.puts(["Hello ", name, "!"])
#  """
#  |> assert_eval()

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

##=======================================================================================================================
## alias, require, import, and use ======================================================================================
##=======================================================================================================================

#  """
#  # Alias the module so it can be called as Bar instead of Foo.Bar
#  alias Foo.Bar, as: Bar
#
#  # Require the module in order to use its macros
#  require Foo
#
#  # Import functions from Foo so they can be called without the `Foo.` prefix
#  import Foo
#
#  # Invokes the custom code defined in Foo as an extension point
#  use Foo
#  """
#  |> assert_eval()

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

#  """
#  defmodule Math do
#    def plus(a, b) do
#      alias Math.List
#      # ...
#    end
#
#    def minus(a, b) do
#      # ...
#    end
#  end
#  """
#  |> assert_eval()

#  """
#  Integer.is_odd(3)
#  ** (UndefinedFunctionError) function Integer.is_odd/1 is undefined or private. However, there is a macro with the same name and arity. Be sure to require Integer if you intend to invoke this macro
#      (elixir) Integer.is_odd(3)
#  """
#  |> assert_eval()

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

#  todo problem with loading the module
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

#  """
#  defmodule Example do
#    use Feature, option: :value
#  end
#  """
#  |> assert_eval_module()

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

#  todo define module inside module does not work
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

##=======================================================================================================================
## Module attributes ====================================================================================================
##=======================================================================================================================

#  todo idk how to or if we want to test the following
#  """
#  defmodule MyServer do
#    @moduledoc "My server code."
#  end
#  """
#  |> assert_eval()
#
#  """
#  defmodule Math do
#    @moduledoc """
#    Provides math-related functions.
#
#    ## Examples
#
#        Math.sum(1, 2)
#        3
#
#    \"""
#
#    @doc \"""
#    Calculates the sum of two numbers.
#    \"""
#    def sum(a, b), do: a + b
#  end
#  """
#  |> assert_eval()
#
#  """
#  h Math # Access the docs for the module Math
#  ...
#  h Math.sum # Access the docs for the sum function
#  ...
#  """
#  |> assert_eval()

#  todo this should actually work in theory but doesnt for some reason
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

#  todo idk how to or if we want to test the following
#  """
#  def some_function, do: do_something_with(@example)
#  def another_function, do: do_something_else_with(@example)
#  """
#  |> assert_eval()

#  """
#  def some_function, do: do_something_with(example())
#  def another_function, do: do_something_else_with(example())
#  defp example, do: @example
#  """
#  |> assert_eval()

#  """
#  @hours_in_a_day 24
#  """
#  |> assert_eval()
#
#  """
#  defp hours_in_a_day(), do: 24
#  """
#  |> assert_eval()
#
#  """
#  defp system_config(), do: %{timezone: "Etc/UTC", locale: "pt-BR"}
#  """
#  |> assert_eval()
#
#  """
#  # Inside pattern
#  @default_timezone "Etc/UTC"
#  def shift(@default_timezone), do: ...
#
#  # Inside guards
#  @time_periods [:am, :pm]
#  def shift(time, period) when period in @time_periods, do: ...
#  """
#  |> assert_eval()

#  """
#  defmodule MyTest do
#    use ExUnit.Case, async: true
#
#    @tag :external
#    @tag os: :unix
#    test "contacts external service" do
#      # ...
#    end
#  end
#  """
#  |> assert_eval()


#=======================================================================================================================
# Structs ==============================================================================================================
#=======================================================================================================================

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

#  """
#  defmodule User do
#    defstruct name: "John", age: 27
#  end
#  """
#  |> assert_eval()
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
  
#  todo
#
#  """
#  john = %User{}
#  %User{age: 27, name: "John"}
#  john[:name]
#  ** (UndefinedFunctionError) function User.fetch/2 is undefined (User does not implement the Access behaviour)
#               User.fetch(%User{age: 27, name: "John"}, :name)
#  Enum.each(john, fn {field, value} -> IO.puts(value) end)
#  ** (Protocol.UndefinedError) protocol Enumerable not implemented for %User{age: 27, name: "John"} of type User (a struct)
#  """
#  |> assert_eval()

#  todo structs definition seems to not work
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

#  todo assert syntax error
#  """
#  defmodule User do
#    defstruct [name: "John", age: 27, :email]
#  end
#  ** (SyntaxError) iex:107: unexpected expression after keyword list. Keyword lists must always come last in lists and maps.
#  """
#  |> assert_eval()
#
#  """
#  defmodule Car do
#    @enforce_keys [:make]
#    defstruct [:model, :make]
#  end
#  %Car{}
#  ** (ArgumentError) the following keys must also be given when building struct Car: [:make]
#      expanding struct: Car.__struct__/1
#  """
#  |> assert_eval()


#=======================================================================================================================
# Protocols ============================================================================================================
#=======================================================================================================================

  """
  defmodule Utility do
    def type(value) when is_binary(value), do: "string"
    def type(value) when is_integer(value), do: "integer"
    # ... other implementations ...
  end
  """
  |> assert_eval_module()
  
#  todo protocols just dont work
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
#  """
#  Enum.map([1, 2, 3], fn x -> x * 2 end)
#  [2, 4, 6]
#  Enum.reduce(1..3, 0, fn x, acc -> x + acc end)
#  6
#  """
#  |> assert_eval()
#
#  """
#  to_string(:hello)
#  "hello"
#  """
#  |> assert_eval()
#
#  """
#  "age: #{25}"
#  "age: 25"
#  """
#  |> assert_eval()
#
#  """
#  tuple = {1, 2, 3}
#  {1, 2, 3}
#  "tuple: #{tuple}"
#  ** (Protocol.UndefinedError) protocol String.Chars not implemented for {1, 2, 3} of type Tuple
#  """
#  |> assert_eval()
#
#  """
#  "tuple: #{inspect(tuple)}"
#  "tuple: {1, 2, 3}"
#  """
#  |> assert_eval()
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
##=======================================================================================================================
## Comprehensions =======================================================================================================
##=======================================================================================================================

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

#  todo File module - idk if we want to test it
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
  |> assert_eval([a: 1, a: 2, b: 1, b: 2, c: 1, c: 2])

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

#  todo IO
#  """
#  stream = IO.stream(:stdio, :line)
#  for line <- stream, into: stream do
#    String.upcase(line) <> "\n"
#  end
#  """
#  |> assert_eval()

#=======================================================================================================================
# Sigils ============================================================================================================
#=======================================================================================================================

#  todo "~r" does not work :(
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

#  todo #{}
#  """
#  ~s(String with escape codes \x26 \#{"inter" <> "polation"})
#  """
#  |> assert_eval("String with escape codes & interpolation")
#
#  """
#  ~S(String without escape codes \x26 without \#{interpolation})
#  """
#  |> assert_eval("String without escape codes \\x26 without \#{interpolation}")

#  todo I doubt we gonna test the "@docs"
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

#  todo "~r"
#  """
#  sigil_r(<<"foo">>, [?i])
#  """
#  |> assert_eval(~r"foo"i)

#  todo "~i"
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

#=======================================================================================================================
# try, catch, and rescue ===============================================================================================
#=======================================================================================================================

#  """
#  :foo + 1
#  ** (ArithmeticError) bad argument in arithmetic expression
#       :erlang.+(:foo, 1)
#  """
#  |> assert_eval()
#
#  """
#  raise "oops"
#  ** (RuntimeError) oops
#  """
#  |> assert_eval()
#
#  """
#  raise ArgumentError, message: "invalid argument foo"
#  ** (ArgumentError) invalid argument foo
#  """
#  |> assert_eval()
#
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

#  todo File module?
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

#  todo #{}
#  """
#  try do
#    Enum.each(-50..50, fn x ->
#      if rem(x, 13) == 0, do: throw(x)
#    end)
#    "Got nothing"
#  catch
#    x -> "Got \#{x}"
#  end
#  """
#  |> assert_eval("Got -39")

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
  
#  """
#  try do
#    raise "fail"
#    what_happened = :did_not_raise
#  rescue
#    _ -> what_happened = :rescued
#  end
#  what_happened
#  ** (CompileError) undefined variable "what_happened"
#  """
#  |> assert_eval()

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

#  """
#  try do
#    raise "fail"
#    another_what_happened = :did_not_raise
#  rescue
#    _ -> another_what_happened
#  end
#  ** (CompileError) undefined variable "another_what_happened"
#  """
#  |> assert_eval()

#=======================================================================================================================
# Writing documentation ================================================================================================
#=======================================================================================================================

#  todo idk if we want to test "@docs"
#  """
#  defmodule MyApp.Hello do
#    @moduledoc """
#    This is the Hello module.
#    """
#    @moduledoc since: "1.0.0"
#
#    @doc """
#    Says hello to the given `name`.
#
#    Returns `:ok`.
#
#    ## Examples
#
#        MyApp.Hello.world(:john)
#        :ok
#
#    """
#    @doc since: "1.3.0"
#    def world(name) do
#      IO.puts("hello #{name}")
#    end
#  end
#  """
#  |> assert_eval()
#
#  """
#  def size(%{size: size}) do
#    size
#  end
#  """
#  |> assert_eval()
#
#  """
#  def size(map_with_size)
#
#  def size(%{size: size}) do
#    size
#  end
#  """
#  |> assert_eval()
#
#  """
#  @doc deprecated: "Use Foo.bar/2 instead"
#  """
#  |> assert_eval()
#
#  """
#  @deprecated "Use Foo.bar/2 instead"
#  """
#  |> assert_eval()
#
#  """
#  @doc group: "Query"
#  def all(query)
#
#  @doc group: "Schema"
#  def insert(schema)
#  """
#  |> assert_eval()
#
#  """
#  @doc since: "1.3.0"
#  def world(name) do
#    IO.puts("hello #{name}")
#  end
#  """
#  |> assert_eval()
#
#  """
#  defmodule MyApp.Hidden do
#    @moduledoc false
#
#    @doc """
#    This function won't be listed in docs.
#    """
#    def function_that_wont_be_listed_in_docs do
#      # ...
#    end
#  end
#  """
#  |> assert_eval()

  """
  defmodule MyApp.Sample do
    @doc false
    def add(a, b), do: a + b
  end
  """
  |> assert_eval_module()

##=======================================================================================================================
## Optional syntax sheet ================================================================================================
##=======================================================================================================================

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

#  todo decide how to test it
#  """
#  if variable? do
#    Call.this()
#  else
#    Call.that()
#  end
#  """
#  |> assert_eval()
#
#  """
#  if variable?, do: Call.this(), else: Call.that()
#  """
#  |> assert_eval()
#  """
#  if variable?, [do: Call.this(), else: Call.that()]
#  """
#  |> assert_eval()
#
#  """
#  if variable?, [{:do, Call.this()}, {:else, Call.that()}]
#  """
#  |> assert_eval()
#
#  """
#  if(variable?, [{:do, Call.this()}, {:else, Call.that()}])
#  """
#  |> assert_eval()

  """
  defmodule(Math, [
    {:do, def(add(a, b), [{:do, a + b}])}
  ])
  """
  |> assert_eval_module()

#=======================================================================================================================
# Erlang libraries =====================================================================================================
#=======================================================================================================================

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

#  """
#  def application do
#    [extra_applications: [:crypto]]
#  end
#  """
#  |> assert_eval()

#  todo :digraph module
#  """
#  digraph = :digraph.new()
#  coords = [{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}]
#  [v0, v1, v2] = (for c <- coords, do: :digraph.add_vertex(digraph, c))
#  :digraph.add_edge(digraph, v0, v1)
#  :digraph.add_edge(digraph, v1, v2)
#  :digraph.get_short_path(digraph, v0, v2)
#  """
#  |> assert_eval([{0.0, 0.0}, {1.0, 0.0}, {1.0, 1.0}])

#  todo :ets module
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

#  todo :rand module
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

#=======================================================================================================================
# Debugging ============================================================================================================
#=======================================================================================================================

#  todo testing IO
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
#  prints:
#  """
#  1..10
#  [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]
#  110
#  """
#
#  """
#  [1, 2, 3]
#  |> IO.inspect(label: "before")
#  |> Enum.map(&(&1 * 2))
#  |> IO.inspect(label: "after")
#  |> Enum.sum
#  """
#  |> assert_eval()
#
#  """
#  before: [1, 2, 3]
#  after: [2, 4, 6]
#  """
#  |> assert_eval()
#
#  """
#  def some_fun(a, b, c) do
#    IO.inspect(binding())
#    ...
#  end
#  """
#  |> assert_eval()
#
#  """
#  [a: :foo, b: "bar", c: :baz]
#  """
#  |> assert_eval()
#
#  """
#  # In my_file.exs
#  feature = %{name: :dbg, inspiration: "Rust"}
#  dbg(feature)
#  dbg(Map.put(feature, :in_version, "1.14.0"))
#  """
#  |> assert_eval()
#
#  """
#  # In dbg_pipes.exs
#  __ENV__.file
#  |> String.split("/", trim: true)
#  |> List.last()
#  |> File.exists?()
#  |> dbg()
#  """
#  |> assert_eval()
#
#  """
#  $ iex
#  :observer.start()
#  """
#  |> assert_eval()

end
