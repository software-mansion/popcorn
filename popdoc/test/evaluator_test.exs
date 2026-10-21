defmodule Popdoc.EvaluatorTest do
  use ExUnit.Case

  test "evaluation sessions" do
    assert %{ok: true, data: %{expressions: [_, _]}} =
             GenServer.call(:main, ["parse_elixir", "x = 40\nx + 2", "block"])

    assert %{ok: true, data: %{result: "40", bindings: [%{name: "x", value: "40"}]}} =
             GenServer.call(:main, ["eval_one", "block", 0])

    assert %{ok: true, data: %{result: "42"}} = GenServer.call(:main, ["eval_one", "block", 1])

    assert %{ok: false, error: %{message: message}} =
             GenServer.call(:main, ["parse_elixir", "(", "invalid"])

    assert is_binary(message)
  end
end
