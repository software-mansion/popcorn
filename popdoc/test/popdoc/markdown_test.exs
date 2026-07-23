defmodule Popdoc.MarkdownTest do
  use ExUnit.Case, async: true

  alias Popdoc.Markdown

  describe "available?/0" do
    test "returns true when ExDoc's Earmark adapter is available" do
      assert Markdown.available?()
    end
  end

  describe "to_ast/2" do
    test "adds the eval class to code blocks marked by elixir-popcorn" do
      ast =
        """
        ```elixir-popcorn
        1 + 2
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert pre_classes(pre) == ["popcorn-eval"]
      assert code_classes(pre) == ["elixir"]
    end

    test "adds the iex class to code blocks marked by iex-popcorn" do
      ast =
        """
        ```iex-popcorn
        iex> x = 1 + 1
        2
        iex> x * 10
        20
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert pre_classes(pre) == ["popcorn-iex"]
      assert code_classes(pre) == ["elixir"]
    end

    test "extracts ordered iex commands into data-popcorn-iex-commands" do
      ast =
        """
        ```iex-popcorn
        iex> x = 1 + 1
        2
        iex> x * 10
        20
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert data_attr(pre, :"data-popcorn-iex-commands") == ~s(["x = 1 + 1","x * 10"])
    end

    test "joins continuation lines into a single command" do
      ast =
        """
        ```iex-popcorn
        iex> x =
        ...>   1 + 2
        3
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert data_attr(pre, :"data-popcorn-iex-commands") == "[\"x =\\n  1 + 2\"]"
    end

    test "recognizes numbered prompts and continuations" do
      ast =
        """
        ```iex-popcorn
        iex(1)> a = 1
        iex(2)> b =
        ...(2)>   2
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert data_attr(pre, :"data-popcorn-iex-commands") == "[\"a = 1\",\"b =\\n  2\"]"
    end

    test "escapes control characters in commands" do
      ast =
        """
        ```iex-popcorn
        iex> IO.puts("a\fb")
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      json = data_attr(pre, :"data-popcorn-iex-commands")
      refute json =~ <<0x0C>>
      assert json == "[\"IO.puts(\\\"a\\fb\\\")\"]"
    end

    test "leaves HTML comments untouched" do
      ast =
        """
        <!-- popcorn:eval -->
        ```elixir
        1 + 2
        ```
        """
        |> Markdown.to_ast([])

      assert [{:comment, [], [" popcorn:eval "], %{comment: true}}, pre] = ast
      assert pre_classes(pre) == []
    end
  end

  defp pre_classes({:pre, attrs, _children, _meta}) do
    attrs
    |> Keyword.get(:class, "")
    |> String.split()
  end

  defp code_classes({:pre, _attrs, [{:code, code_attrs, _children, _code_meta}], _pre_meta}) do
    code_attrs
    |> Keyword.get(:class, "")
    |> String.split()
  end

  defp data_attr({:pre, attrs, _children, _meta}, key) do
    Keyword.get(attrs, key)
  end
end
