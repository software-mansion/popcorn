defmodule FissionLib.Patches.ModuleTest do
  use ExUnit.Case, async: true

  @tested_patch Path.expand("../../../patches/exavmlib/Module.ex", __DIR__)

  test "concat/2" do
    assert module_str =
             File.read!(@tested_patch)
             |> String.replace("defmodule Module do", "defmodule TestModule do", global: false)

    assert [{TestModule, beam}] = Code.compile_string(module_str, @tested_patch)
    assert {:module, module} = :code.load_binary(TestModule, :nofile, beam)

    assert module.concat(Foo, Bar) == Foo.Bar
    assert module.concat(Foo, :Bar) == Foo.Bar
    assert module.concat(Foo, "Bar") == Foo.Bar
    assert module.concat(Foo, Bar.Baz) == Foo.Bar.Baz
    assert module.concat(Foo, "Bar.Baz") == Foo.Bar.Baz
    assert module.concat(Bar, nil) == Elixir.Bar
  end
end
