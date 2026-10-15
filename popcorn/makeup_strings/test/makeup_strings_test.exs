defmodule MakeupStringsTest.JavascriptLexer do
  def lex(text, _opts), do: [{:name, %{language: :javascript}, text}]
end

defmodule MakeupStringsTest do
  use ExUnit.Case

  alias Makeup.Lexer
  alias MakeupStrings.ElixirLexer

  setup_all do
    Makeup.Registry.register_lexer(MakeupStringsTest.JavascriptLexer, names: ["javascript"])

    previous = Application.get_env(:makeup_strings, :languages)

    Application.put_env(:makeup_strings, :languages,
      javascript: [
        {Popcorn.JS, :run, 3},
        {Popcorn.JS, :run!, 3}
      ]
    )

    on_exit(fn -> Application.put_env(:makeup_strings, :languages, previous) end)
  end

  describe "configured calls" do
    test "highlights every direct string argument through the configured arity" do
      source = ~S'''
      Popcorn.JS.run("one")
      Popcorn.JS.run("two", %{})
      Popcorn.JS.run("three", "also three", %{label: "nested"})
      Popcorn.JS.run!("bang")

      alias Popcorn.JS
      JS.run!("aliased")
      '''

      tokens = ElixirLexer.lex(source, group_prefix: "group")

      assert embedded(tokens) == ["one", "two", "three", "also three", "bang", "aliased"]

      assert Enum.count(tokens, &match?({:string, %{group_id: "makeup-string-start"}, _}, &1)) ==
               6

      assert Enum.count(tokens, &match?({:string, %{group_id: "makeup-string-end"}, _}, &1)) == 6
      assert Lexer.unlex(tokens) == source
    end

    test "leaves non-matches and incomplete strings alone" do
      source = ~S'''
      Other.run("wrong module")
      Popcorn.JS.other("wrong function")
      Popcorn.JS.run("one", "two", "three", "too many")
      Popcorn.JS.run("const #{name} = true")
      '''

      tokens = ElixirLexer.lex(source, group_prefix: "group")

      assert embedded(tokens) == []
      assert Lexer.unlex(tokens) == source
    end

    test "pipes" do
      source = ~S'''
      "inline" |> Popcorn.JS.run()
      "separate"
      |> Popcorn.JS.run!(%{})
      """
      const value = 1;
      """
      |> Popcorn.JS.run("explicit", %{})
      Popcorn.JS.run("nested" |> Popcorn.JS.run!())
      alias Popcorn.JS
      "aliased pipe" |> JS.run!()
      '''

      tokens = ElixirLexer.lex(source, group_prefix: "group")

      assert embedded(tokens) == [
               "inline",
               "separate",
               "\nconst value = 1;\n",
               "explicit",
               "nested",
               "aliased pipe"
             ]

      assert Lexer.unlex(tokens) == source
    end

    test "pipe boundaries" do
      source = ~S'''
      "wrong module" |> Other.run()
      "wrong function" |> Popcorn.JS.other()
      "too many" |> Popcorn.JS.run("one", "two", "three")
      "interpolated #{name}" |> Popcorn.JS.run()
      "indirect" |> String.trim() |> Popcorn.JS.run()
      %{label: "nested"} |> Popcorn.JS.run()
      value |> Popcorn.JS.run("one", "two", "three")
      '''

      tokens = ElixirLexer.lex(source, group_prefix: "group")

      assert embedded(tokens) == []
      assert Lexer.unlex(tokens) == source
    end

    test "accepts configuration in lexer options" do
      source = ~S|Example.render("configured")|

      tokens =
        ElixirLexer.lex(source,
          group_prefix: "group",
          languages: [javascript: [{Example, :render, 1}]]
        )

      assert embedded(tokens) == ["configured"]
      assert Lexer.unlex(tokens) == source
    end
  end

  test "registers the composite Elixir lexer" do
    assert {:ok, {ElixirLexer, []}} = Makeup.Registry.fetch_lexer_by_name("elixir")
  end

  defp embedded(tokens) do
    for {:name, %{language: :javascript}, value} <- tokens, do: value
  end
end
