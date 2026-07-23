defmodule PopdocWasm.IexSessionTest do
  use ExUnit.Case, async: true

  alias PopdocWasm.IexSession

  defp new, do: IexSession.new()

  describe "incomplete input" do
    test "dangling operator" do
      session = new()
      assert {:incomplete, ^session} = IexSession.eval(session, "1 +")
    end

    test "missing end" do
      session = new()
      assert {:incomplete, ^session} = IexSession.eval(session, "defmodule Foo do")
    end

    test "unclosed list" do
      session = new()
      assert {:incomplete, ^session} = IexSession.eval(session, "[1, 2")
    end

    test "unclosed string" do
      session = new()
      assert {:incomplete, ^session} = IexSession.eval(session, ~s("abc))
    end

    test "heredoc opener without trailing newline" do
      session = new()
      assert {:incomplete, ^session} = IexSession.eval(session, ~s(@doc """))
      assert {:incomplete, ^session} = IexSession.eval(session, ~s(x = """))
    end

    test "completed heredoc evaluates" do
      assert {:ok, result, _} =
               IexSession.eval(new(), "x = \"\"\"\nhello\n\"\"\"")

      assert result =~ "hello"
    end

    test "does not touch counter or binding" do
      {:ok, _, session} = IexSession.eval(new(), "x = 1")
      assert {:incomplete, unchanged} = IexSession.eval(session, "x +")
      assert unchanged.counter == session.counter
      assert unchanged.binding == session.binding
    end
  end

  describe "parse errors" do
    test "syntax error bumps the counter" do
      assert {:error, error, session} = IexSession.eval(new(), "1 )")
      assert error.type in ["SyntaxError", "MismatchedDelimiterError"]
      assert error.stacktrace == ""
      assert session.counter == 2
    end
  end

  describe "evaluation" do
    test "simple expression" do
      assert {:ok, "2", session} = IexSession.eval(new(), "1 + 1")
      assert session.counter == 2
    end

    test "bindings persist across evals" do
      {:ok, "41", session} = IexSession.eval(new(), "x = 41")
      assert {:ok, "42", _} = IexSession.eval(session, "x + 1")
    end

    test "rebinding works" do
      {:ok, _, session} = IexSession.eval(new(), "x = 1")
      {:ok, _, session} = IexSession.eval(session, "x = 2")
      assert {:ok, "2", _} = IexSession.eval(session, "x")
    end

    test "inspect opts colorize the result" do
      assert {:ok, result, _} =
               IexSession.eval(new(), ":ok", syntax_colors: IO.ANSI.syntax_colors())

      assert result =~ "\e["
      assert result =~ ":ok"
    end

    test "multi-expression text returns the last value" do
      assert {:ok, "2", _} = IexSession.eval(new(), "a = 1\na + 1")
    end

    test "completed continuation bumps the counter once" do
      session = new()
      {:incomplete, session} = IexSession.eval(session, "if true do")
      {:incomplete, session} = IexSession.eval(session, "if true do\n1")
      assert {:ok, "1", session} = IexSession.eval(session, "if true do\n1\nend")
      assert session.counter == 2
    end
  end

  describe "runtime errors" do
    test "exception maps to error with type and bumps counter" do
      assert {:error, error, session} = IexSession.eval(new(), "1 / 0")
      assert error.type == "ArithmeticError"
      assert error.kind == :error
      assert session.counter == 2
    end

    test "bindings survive a failed eval" do
      {:ok, _, session} = IexSession.eval(new(), "x = 5")
      {:error, _, session} = IexSession.eval(session, "raise \"boom\"")
      assert {:ok, "5", _} = IexSession.eval(session, "x")
    end

    test "throw maps to kind :throw" do
      assert {:error, error, _} = IexSession.eval(new(), "throw :ball")
      assert error.kind == :throw
      assert error.type == nil
      assert error.message == ":ball"
    end
  end
end
