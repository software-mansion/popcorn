defmodule FissionLib.Support.AsyncTest do
  use ExUnit.Case, async: true
  alias FissionLib.Support.AtomVM
  @moduledoc false
  # Helper for creating asynchronous tests
  # - creates a public function instead of a test
  # - creates a module with a single test that calls said function
  # - copies all @tags to the newly created module
  # - setup and setup_all won't work (yet)

  defmacro async_test(
             test_name,
             context \\ quote do
               %{}
             end,
             do: block
           ) do
    id =
      :erlang.term_to_binary(block)
      |> then(&:crypto.hash(:sha256, &1))
      |> Base.encode16(case: :lower)
      |> String.slice(0..12)

    test_module_name = Module.concat(__CALLER__.module, "AsyncTest_#{id}")
    fun_name = :"async_test_#{id}"
    after_compile_fun_name = :"async_test_ac_#{id}"

    quote do
      @tags_attrs [:tag, :describetag, :moduletag]
                  |> Enum.flat_map(fn attr ->
                    Module.get_attribute(__MODULE__, attr)
                    |> Enum.map(&{attr, &1})
                  end)

      def unquote(fun_name)(unquote(context)) do
        unquote(block)
      end

      def unquote(after_compile_fun_name)(_bytecode, _env) do
        test_name = unquote(test_name)
        fun_name = unquote(fun_name)

        content =
          quote do
            use ExUnit.Case, async: true

            Enum.each(unquote(@tags_attrs), fn {name, value} ->
              Module.put_attribute(__MODULE__, name, value)
            end)

            test unquote(test_name), context do
              unquote(__MODULE__).unquote(fun_name)(context)
            end
          end

        Module.create(unquote(test_module_name), content, __ENV__)
      end

      @after_compile {__MODULE__, unquote(after_compile_fun_name)}

      Module.delete_attribute(__MODULE__, :tag)
    end
  end

  defmacro assert_eval(code_string, expected) do
    quote do
      async_test "evaluation", %{tmp_dir: dir} do
        result =
          unquote(code_string)
          |> AtomVM.eval(:elixir, run_dir: dir)
        unquote(__MODULE__).assert_expectation(unquote(expected), result)
      end
    end
  end
  
  defmacro assert_eval_module(code_string) do
    quote do
      async_test "evaluation", %{tmp_dir: dir} do
        result =
          unquote(code_string)
          |> AtomVM.eval(:elixir, run_dir: dir)
          |> AtomVM.assert_is_module()
      end
    end
  end

  def assert_expectation({:expect_fn, f}, result) when is_function(f) do
    assert f.(result)
  end

  def assert_expectation(e, result) do
    assert e === result
  end
end
