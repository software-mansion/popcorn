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

  describe "on_load" do
    # The on_load functions below record their runs in :persistent_term;
    # patch_and_load/3 clears the record before loading the merged module.
    test "original's private on_load is kept", %{tmp_dir: tmp_dir} do
      module =
        patch_and_load(
          quote do
            @on_load :orig_init
            defp orig_init(), do: unquote(record(:orig))
            def hello(), do: :hello
          end,
          quote do
            def hello(), do: :patch_hello
          end,
          tmp_dir
        )

      assert [:orig] = on_load_runs(module)
      assert :patch_hello = module.hello()
    end

    test "patch's on_load is kept", %{tmp_dir: tmp_dir} do
      module =
        patch_and_load(
          quote do
            def hello(), do: :hello
          end,
          quote do
            @on_load :patch_init
            defp patch_init(), do: unquote(record(:patch))
          end,
          tmp_dir
        )

      assert [:patch] = on_load_runs(module)
    end

    test "patch's function with the same name becomes the on_load", %{tmp_dir: tmp_dir} do
      module =
        patch_and_load(
          quote do
            @on_load :init
            defp init(), do: unquote(record(:orig))
          end,
          quote do
            @compile {:no_warn_undefined, :popcorn_module}
            defp init(), do: unquote(record(:patch))
            def call_init(), do: init()
            def call_orig_init(), do: :popcorn_module.init()
          end,
          tmp_dir
        )

      assert [:patch] = on_load_runs(module)
      assert :ok = module.call_init()
      # The original's function is still there under the renamed name
      assert :ok = module.call_orig_init()
      assert [:patch, :patch, :orig] = on_load_runs(module)
    end

    test "patch's override of the on_load function becomes the on_load", %{tmp_dir: tmp_dir} do
      module =
        patch_and_load(
          quote do
            @on_load :init
            defp init(), do: unquote(record(:orig))
          end,
          quote do
            @compile {:popcorn_patch_private, init: 0}
            def init(), do: unquote(record(:patch))
          end,
          tmp_dir
        )

      assert [:patch] = on_load_runs(module)
      refute function_exported?(module, :init, 0)
    end

    test "patch can call the original's on_load function", %{tmp_dir: tmp_dir} do
      module =
        patch_and_load(
          quote do
            @on_load :init
            defp init(), do: unquote(record(:orig))
          end,
          quote do
            @compile {:no_warn_undefined, :popcorn_module}
            def call_orig_init(), do: :popcorn_module.init()
          end,
          tmp_dir
        )

      assert [:orig] = on_load_runs(module)
      assert :ok = module.call_orig_init()
      assert [:orig, :orig] = on_load_runs(module)
    end
  end

  defp record(who) do
    quote do
      key = {:on_load_runs, __MODULE__}
      :persistent_term.put(key, :persistent_term.get(key, []) ++ [unquote(who)])
      :ok
    end
  end

  defp on_load_runs(module), do: :persistent_term.get({:on_load_runs, module}, [])

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
      CoreErlangUtils.merge_modules(
        CoreErlangUtils.parse(orig),
        CoreErlangUtils.parse(patch)
      )
      |> CoreErlangUtils.serialize()

    # Compiling above already ran the on_load functions of both modules
    :persistent_term.erase({:on_load_runs, module})
    assert {:module, ^module} = :code.load_binary(module, ~c"#{inspect(module)}.ex", beam)
    module
  end
end
