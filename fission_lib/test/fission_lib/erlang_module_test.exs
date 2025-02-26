defmodule FissionLib.ErlangModuleTest do
  use ExUnit.Case, async: true
  require Logger
  import FissionLib.Support.AsyncTest
  alias FissionLib.Support.AtomVM

  @moduletag :tmp_dir
  @examples_path "./test/examples"

  async_test "Adder", %{tmp_dir: dir} do
    """
    -module(adder).
    -export([add/2]).

    add(A, B) ->
        A + B.
    """
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "Long Function - function length check", %{tmp_dir: dir} do
    "#{@examples_path}/long_function.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "UUID - too big literals", %{tmp_dir: dir} do
    "#{@examples_path}/uuid.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "Greetings - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/greetings.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end

  async_test "Capybara habitat - message passing", %{tmp_dir: dir} do
    "#{@examples_path}/capybara_habitat.erl"
    |> File.read!()
    |> AtomVM.eval(:erlang_module, run_dir: dir)
    |> AtomVM.assert_is_module()
  end
end
