defmodule FissionLib.ErlangModuleTest do
  use ExUnit.Case, async: true
  require Logger
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @examples_path "./test/examples"

  async_test "Adder" do
    """
    -module(adder).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  async_test "UUID - too big literals" do
    "#{@examples_path}/uuid.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  async_test "Greetings - message passing" do
    "#{@examples_path}/greetings.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end

  async_test "Capybara habitat - message passing" do
    "#{@examples_path}/capybara_habitat.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang)
    |> AtomVM.assert_is_module()
  end
end
