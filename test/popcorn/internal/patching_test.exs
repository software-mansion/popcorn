defmodule Popcorn.Internal.PatchingTest do
  # Test is sync due to global compile options being changed
  use ExUnit.Case, async: false

  alias Popcorn.CoreErlangUtils

  @moduletag :tmp_dir

  setup_all do
    debug_info = Code.get_compiler_option(:debug_info)
    ignore_module_conflict = Code.get_compiler_option(:ignore_module_conflict)

    # By default, Elixir doesn't store debug info in test env to speed up compilation
    Code.put_compiler_option(:debug_info, true)
    Code.put_compiler_option(:ignore_module_conflict, true)

    on_exit(fn ->
      Code.put_compiler_option(:debug_info, debug_info)
      Code.put_compiler_option(:ignore_module_conflict, ignore_module_conflict)
    end)
  end

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
          @compile {:no_warn_undefined, :popcorn_module}
          def pub_hello(x), do: priv_foo({:patch_hello, x, priv_bar()})
          defp priv_foo(x), do: {:yay, x}
          defp priv_bar(), do: :patch_bar
          @compile {:popcorn_patch_private, priv_baz: 0}
          def priv_baz(), do: :patch_baz
          @compile {:popcorn_patch_private, priv_baz2: 0}
          def priv_baz2(), do: :patch_baz2
          def priv_baz3(), do: :patch_baz3
          def pub_call_ups(), do: pub_ups()
          defp pub_ups(), do: :patch_ups
          def pub_call_orig_bar(), do: :popcorn_module.priv_bar()
          def pub_call_orig_baz4(), do: :popcorn_module.priv_baz4()
          def pub_call_hello_popcorn_module(x), do: :popcorn_module.pub_hello(x)
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

    assert {:yay, {:patch_hello, :world2, :patch_bar}} =
             module.pub_call_hello_popcorn_module(:world2)

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
      |> Code.compile_quoted(tmp_dir)

    [{_module, patch}] =
      quote do
        defmodule unquote(module) do
          unquote(patch)
        end
      end
      |> Code.compile_quoted(tmp_dir)

    beam =
      CoreErlangUtils.merge_modules(CoreErlangUtils.parse(orig), CoreErlangUtils.parse(patch))
      |> CoreErlangUtils.serialize()

    assert {:module, ^module} = :code.load_binary(module, ~c"#{inspect(module)}.ex", beam)
    module
  end
end
