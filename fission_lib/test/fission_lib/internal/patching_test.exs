defmodule FissionLib.Internal.PatchingTest do
  use ExUnit.Case, async: true

  alias FissionLib.CoreErlangUtils

  @moduletag :tmp_dir

  @tag :target
  test "patching", %{tmp_dir: tmp_dir} do
    module =
      patch_and_load(
        quote do
          def pub_hello(x), do: priv_foo({:hello, x, priv_bar()})
          def pub_blah(x), do: priv_foo({:blah, x, priv_baz(), priv_baz2(), priv_baz3()})
          def pub_ups(), do: :ups
          def pub_call_baz4(), do: priv_baz4()
          defp priv_foo(x), do: {:ok, x}
          defp priv_bar(), do: :bar
          defp priv_baz(), do: :baz
          defp priv_baz2(), do: :baz2
          defp priv_baz3(), do: :baz3
          defp priv_baz4(), do: :baz4
        end,
        quote do
          @compile {:no_warn_undefined, :flb_priv}
          def pub_hello(x), do: priv_foo({:patch_hello, x, priv_bar()})
          defp priv_foo(x), do: {:yay, x}
          defp priv_bar(), do: :patch_bar
          @compile {:flb_patch_private, priv_baz: 0}
          def priv_baz(), do: :patch_baz
          @compile {:flb_patch_private, priv_baz2: 0}
          def priv_baz2(), do: :patch_baz2
          def priv_baz3(), do: :patch_baz3
          def pub_call_ups(), do: pub_ups()
          defp pub_ups(), do: :patch_ups
          def pub_call_orig_bar(), do: :flb_priv.priv_bar()
          def pub_call_orig_baz4(), do: :flb_priv.priv_baz4()
        end,
        tmp_dir
      )

    assert {:yay, {:patch_hello, :world, :patch_bar}} = module.pub_hello(:world)
    assert {:ok, {:blah, :boom, :patch_baz, :patch_baz2, :baz3}} = module.pub_blah(:boom)
    assert :patch_baz3 = module.priv_baz3()
    assert :ups = module.pub_ups()
    assert :patch_ups = module.pub_call_ups()
    assert :patch_ups = module.pub_call_ups()
    assert :bar = module.pub_call_orig_bar()
    assert :baz4 = module.pub_call_orig_baz4()
    refute function_exported?(module, :priv_baz, 0)
    refute function_exported?(module, :priv_baz2, 0)
  end

  defp patch_and_load(orig, patch, tmp_dir) do
    module = String.to_atom("#{__MODULE__.Foo}#{:erlang.unique_integer([:positive])}")

    [{_module, orig}] =
      quote do
        defmodule unquote(module) do
          unquote(orig)
        end
      end
      |> Utils.compile_quoted(tmp_dir)

    [{_module, patch}] =
      quote do
        defmodule unquote(module) do
          unquote(patch)
        end
      end
      |> Utils.compile_quoted(tmp_dir)

    beam =
      CoreErlangUtils.merge_modules(CoreErlangUtils.parse(orig), CoreErlangUtils.parse(patch))
      |> CoreErlangUtils.serialize()

    assert {:module, ^module} = :code.load_binary(module, ~c"#{inspect(module)}.ex", beam)
    module
  end
end
