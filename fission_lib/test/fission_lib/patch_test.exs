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
          ("try do\n" <>
             (@additional_prep <> unquote(code_string)) <>
             """
             rescue
               e -> e
             end
             """)
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

  """
  Module.concat(Foo, Bar)
  """
  |> assert_eval(Foo.Bar)

  """
  Module.concat(Foo, :Bar)
  """
  |> assert_eval(Foo.Bar)

  """
  Module.concat(Foo, "Bar")
  """
  |> assert_eval(Foo.Bar)

  """
    Module.concat(Foo, Bar.Baz)
  """
  |> assert_eval(Foo.Bar.Baz)

  """
  Module.concat(Foo, "Bar.Baz")
  """
  |> assert_eval(Foo.Bar.Baz)

  """
    Module.concat(Bar, nil)
  """
  |> assert_eval(Elixir.Bar)
end
