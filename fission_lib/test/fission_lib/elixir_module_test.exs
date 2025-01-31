defmodule FissionLib.ElixirModuleTest do
  use ExUnit.Case, async: true
  require Logger
  require FissionLib.AtomVM
  import FissionLib.AsyncTest
  alias FissionLib.AtomVM

  @examples_path "./test/examples"

  async_test "simple module" do
    """
    defmodule Start do

    end
    """
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_is_module()
  end

  async_test "Capybara habitat - genserver" do
    "#{@examples_path}/CapybaraHabitat.ex"
    |> File.read!()
    |> AtomVM.eval(:elixir)
    |> AtomVM.assert_is_module()
  end
end
