defmodule FissionLib.Patches.ModuleTest do
  use ExUnit.Case, async: true
  import FissionLib.Support.AsyncTest

  @moduletag :tmp_dir

  defmacrop eval(dir, code) do
    code_string = Macro.to_string(code[:do])

    quote do
      require FissionLib.Support.AtomVM
      FissionLib.Support.AtomVM.eval(unquote(code_string), :elixir, run_dir: unquote(dir))
    end
  end

  async_test "Module.concat", %{tmp_dir: dir} do
    eval dir do
      Module.concat(Foo, Bar)
    end
    |> then(&assert &1 === Foo.Bar)

    eval dir do
      Module.concat(Foo, :Bar)
    end
    |> then(&assert &1 === Foo.Bar)

    eval dir do
      Module.concat(Foo, "Bar")
    end
    |> then(&assert &1 === Foo.Bar)

    eval dir do
      Module.concat(Foo, Bar.Baz)
    end
    |> then(&assert &1 === Foo.Bar.Baz)

    eval dir do
      Module.concat(Foo, "Bar.Baz")
    end
    |> then(&assert &1 === Foo.Bar.Baz)

    eval dir do
      Module.concat(Bar, nil)
    end
    |> then(&assert &1 === Elixir.Bar)
  end
end
