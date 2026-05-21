defmodule Popdoc.MarkdownTest do
  use ExUnit.Case, async: true

  alias Popdoc.Markdown

  describe "available?/0" do
    test "returns true when ExDoc's Earmark adapter is available" do
      assert Markdown.available?()
    end
  end

  describe "to_ast/2" do
    test "adds the eval class to code blocks marked by the HTML comment" do
      ast =
        """
        <!-- popcorn:eval -->
        ```elixir
        1 + 2
        ```
        """
        |> Markdown.to_ast([])

      assert [pre] = ast
      assert pre_classes(pre) == ["popcorn-eval"]
    end

    test "leaves unrelated comments untouched" do
      ast =
        """
        <!-- not popcorn -->
        ```elixir
        1 + 2
        ```
        """
        |> Markdown.to_ast([])

      assert [{:comment, [], [" not popcorn "], %{comment: true}}, pre] = ast
      assert pre_classes(pre) == []
    end
  end

  defp pre_classes({:pre, attrs, _children, _meta}) do
    attrs
    |> Keyword.get(:class, "")
    |> String.split()
  end
end
