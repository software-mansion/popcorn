defmodule FissionLib.ErlangModuleTest do
  use ExUnit.Case, async: true
  require Logger
  alias FissionLib.AtomVM

  @examples_path "./test/examples"

  test "Adder" do
    """
    -module(adder).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  test "UUID - too big literals" do
    "#{@examples_path}/uuid.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  test "Greetings - message passing" do
    "#{@examples_path}/greetings.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  test "Capybara habitat - message passing" do
    "#{@examples_path}/capybara_habitat.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end
end
