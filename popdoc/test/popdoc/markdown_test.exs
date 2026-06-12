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
end
