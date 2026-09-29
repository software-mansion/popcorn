defmodule PopdocWasm.EvalTest do
  use ExUnit.Case, async: true

  alias PopdocWasm.Eval

  defp eval(code, binding \\ [], opts \\ []) do
    Eval.eval_string(code, binding, Eval.fresh_env("test"), opts)
  end

  describe "incomplete input" do
    test "dangling operator" do
      assert :incomplete = eval("1 +")
    end

    test "missing end" do
      assert :incomplete = eval("defmodule Foo do")
    end

    test "unclosed list" do
      assert :incomplete = eval("[1, 2")
    end

    test "unclosed string" do
      assert :incomplete = eval(~s("abc))
    end

    test "heredoc opener without trailing newline" do
      assert :incomplete = eval(~s(@doc """))
      assert :incomplete = eval(~s(x = """))
    end

    test "completed heredoc evaluates" do
      assert {:ok, result, _, _} = eval("x = \"\"\"\nhello\n\"\"\"")
      assert result =~ "hello"
    end
  end

  describe "parse errors" do
    test "syntax error maps to error with empty stacktrace" do
      assert {:error, error} = eval("1 )")
      assert error.type in ["SyntaxError", "MismatchedDelimiterError"]
      assert error.stacktrace == ""
    end
  end

  describe "evaluation" do
    test "simple expression" do
      assert {:ok, "2", _, _} = eval("1 + 1")
    end

    test "bindings persist across evals" do
      {:ok, "41", binding, env} = eval("x = 41")
      assert {:ok, "42", _, _} = Eval.eval_string("x + 1", binding, env)
    end

    test "rebinding works" do
      {:ok, _, binding, env} = eval("x = 1")
      {:ok, _, binding, env} = Eval.eval_string("x = 2", binding, env)
      assert {:ok, "2", _, _} = Eval.eval_string("x", binding, env)
    end

    test "inspect opts colorize the result" do
      assert {:ok, result, _, _} = eval(":ok", [], syntax_colors: IO.ANSI.syntax_colors())
      assert result =~ "\e["
      assert result =~ ":ok"
    end

    test "multi-expression text returns the last value" do
      assert {:ok, "2", _, _} = eval("a = 1\na + 1")
    end

    test "continuation completes once the input closes" do
      assert :incomplete = eval("if true do")
      assert :incomplete = eval("if true do\n1")
      assert {:ok, "1", _, _} = eval("if true do\n1\nend")
    end
  end

  describe "runtime errors" do
    test "exception maps to error with type" do
      assert {:error, error} = eval("1 / 0")
      assert error.type == "ArithmeticError"
      assert error.kind == :error
    end

    test "bindings survive a failed eval" do
      {:ok, _, binding, env} = eval("x = 5")
      assert {:error, _} = Eval.eval_string("raise \"boom\"", binding, env)
      assert {:ok, "5", _, _} = Eval.eval_string("x", binding, env)
    end

    test "throw maps to kind :throw" do
      assert {:error, error} = eval("throw :ball")
      assert error.kind == :throw
      assert error.type == nil
      assert error.message == ":ball"
    end
  end
end
