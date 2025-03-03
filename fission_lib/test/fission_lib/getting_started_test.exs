defmodule FissionLib.GettingStartedTest do
  use ExUnit.Case, async: true
  require FissionLib.Support.AtomVM
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir

  defmacro assert_eval(line, expected) do
    quote do
      result =
        unquote(line)
        |> AtomVM.eval(:elixir, run_dir: var!(dir))

      assert unquote(expected) === result
    end
  end

  defmacro assert_eval_type(line, type) do
    {m,f} =
      case type do
        :string -> {String, :valid?}
        _ -> {Kernel, :"is_#{type}"}
      end
    quote do
      result =
        unquote(line)
        |> AtomVM.eval(:elixir, run_dir: var!(dir))
        |> then(fn(x) -> apply(unquote(m), unquote(f), [x]) end)
      AtomVM.assert_result(result, true)
    end
  end
  
  async_test "introduction", %{tmp_dir: dir} do
    assert_eval("40 + 2\n", 42)
    """
    "hello" <> " world"
    """
    |> assert_eval("hello world")
  end
  
  async_test "basic types", %{tmp_dir: dir} do
    
    assert_eval_type("1\n", :integer)
    assert_eval_type("1\n", :integer)
    assert_eval_type("0x1F\n", :integer)
    assert_eval_type("1.0\n", :float)
    assert_eval_type("true\n", :boolean)
    assert_eval_type(":atom\n", :atom)
    assert_eval_type("\"elixir\"\n", :string)
    assert_eval_type("[1, 2, 3]\n", :list)
    assert_eval_type("{1, 2, 3}\n", :tuple)

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

#    todo assert_eval("1 and true\n", "** (BadBooleanError) expected a boolean on left-side of "and", got: 1")

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
#
    assert_eval(":apple == :apple\n", true)
    assert_eval(":apple == :orange\n", false)
#
    assert_eval("true == :true\n", true)
    assert_eval("is_atom(false)\n", true)
    assert_eval("is_boolean(:false)\n", true)
#
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
   
#
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
  end

  async_test "lists and tuples", %{tmp_dir: dir} do
    assert_eval("[1, 2, true, 3]\n", [1, 2, true, 3])
    assert_eval("length([1, 2, 3])\n", 3)
    
    assert_eval("[1, 2, 3] ++ [4, 5, 6]\n", [1, 2, 3, 4, 5, 6])
    assert_eval("[1, true, 2, false, 3, true] -- [true, false]\n", [1, 2, 3, true])
    
    """
    list = [1, 2, 3]
    hd(list)
    """
    |> assert_eval(1)
#    "tl(list)\n", [2, 3]

#    todo hd([]) "** (ArgumentError) argument error"

    assert_eval("[11, 12, 13]\n", ~c"\v\f\r")
    assert_eval("[104, 101, 108, 108, 111]\n", ~c"hello")
#
    assert_eval("{:ok, \"hello\"}\n", {:ok, "hello"})
    assert_eval("tuple_size({:ok, \"hello\"})", 2)

#    tuple = {:ok, "hello"}
#    {:ok, "hello"}
#    elem(tuple, 1)
#    "hello"
#    tuple_size(tuple)
#    2
#
#    tuple = {:ok, "hello"}
#    {:ok, "hello"}
#    put_elem(tuple, 1, "world")
#    {:ok, "world"}
#    tuple
#    {:ok, "hello"}
#
#    list = [1, 2, 3]
#    [1, 2, 3]
#
#    # This is fast as we only need to traverse `[0]` to prepend to `list`
#    [0] ++ list
#    [0, 1, 2, 3]
#
#    # This is slow as we need to traverse `list` to append 4
#    list ++ [4]
#    [1, 2, 3, 4]
#
#    tuple = {:a, :b, :c, :d}
#    {:a, :b, :c, :d}
#    put_elem(tuple, 2, :e)
#    {:a, :b, :e, :d}
#
#    String.split("hello world")
#    ["hello", "world"]
#    String.split("hello beautiful world")
#    ["hello", "beautiful", "world"]
#
#    String.split_at("hello world", 3)
#    {"hel", "lo world"}
#    String.split_at("hello world", -4)
#    {"hello w", "orld"}
#
#    File.read("path/to/existing/file")
#    {:ok, "... contents ..."}
#    File.read("path/to/unknown/file")
#    {:error, :enoent}
#
#    tuple = {:ok, "hello"}
#    {:ok, "hello"}
#    elem(tuple, 1)
#    "hello"
    
    
  end
end
