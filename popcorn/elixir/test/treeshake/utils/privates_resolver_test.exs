defmodule Treeshake.Utils.PrivatesResolverTest do
  use ExUnit.Case, async: true

  alias Treeshake.Utils.PrivatesResolver

  @moduletag :treeshake

  # ---- helpers ----

  defp build_info(functions, opts \\ []) do
    %{
      module: opts[:module] || :TestModule,
      functions: functions,
      abstraction: opts[:abstraction],
      behaviour_impls: opts[:behaviour_impls] || [],
      protocol_impl: opts[:protocol_impl] || []
    }
  end

  defp pub(name, arity, calls \\ [], potential_modules \\ []) do
    %{
      name: name,
      arity: arity,
      public: true,
      calls: calls,
      potential_modules: potential_modules
    }
  end

  defp priv(name, arity, calls \\ [], potential_modules \\ []) do
    %{
      name: name,
      arity: arity,
      public: false,
      calls: calls,
      potential_modules: potential_modules
    }
  end

  defp find_pub(result, name, arity \\ 0), do: result.public_functions[{name, arity}]
  defp find_priv(result, name, arity \\ 0), do: result.private_functions[{name, arity}]

  # ---- tests ----

  describe "analyze/1 - structure" do
    test "returns module atom unchanged" do
      result = PrivatesResolver.resolve(build_info([], module: :MyMod))
      assert result.module == :MyMod
    end

    test "passes through abstraction from input" do
      assert PrivatesResolver.resolve(build_info([], abstraction: {:protocol, []})).abstraction ==
               {:protocol, []}

      assert PrivatesResolver.resolve(build_info([], abstraction: {:behaviour, [foo: 0]})).abstraction ==
               {:behaviour, [foo: 0]}

      assert PrivatesResolver.resolve(build_info([])).abstraction == nil
    end

    test "empty module produces empty maps" do
      result = PrivatesResolver.resolve(build_info([]))
      assert result.public_functions == %{}
      assert result.private_functions == %{}
    end

    test "public functions are keyed by {name, arity}" do
      fns = [pub(:foo, 0), priv(:bar, 0)]
      result = PrivatesResolver.resolve(build_info(fns))
      assert %{} = result.public_functions[{:foo, 0}]
      assert Map.has_key?(result.private_functions, {:bar, 0})
    end
  end

  describe "analyze/1 - public function call expansion" do
    test "public function with no calls keeps empty fields" do
      result = PrivatesResolver.resolve(build_info([pub(:foo, 0)]))
      info = find_pub(result, :foo)
      assert info.calls == []
      assert info.potential_modules == []
    end

    test "remote calls are preserved unchanged" do
      calls = [{String, :upcase, 1}, {Enum, :map, 2}]
      result = PrivatesResolver.resolve(build_info([pub(:foo, 0, calls)]))
      info = find_pub(result, :foo)
      assert {String, :upcase, 1} in info.calls
      assert {Enum, :map, 2} in info.calls
    end

    test "calls to private functions are excluded" do
      fns = [pub(:foo, 0, [{nil, :helper, 0}]), priv(:helper, 0)]
      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      refute {:TestModule, :helper, 0} in info.calls
    end

    test "calls of a directly-called private function are merged in" do
      fns = [
        pub(:foo, 0, [{nil, :helper, 0}]),
        priv(:helper, 0, [{String, :upcase, 1}])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      assert {String, :upcase, 1} in info.calls
    end

    test "potential_modules of a directly-called private function are merged in" do
      fns = [
        pub(:foo, 0, [{nil, :helper, 0}]),
        priv(:helper, 0, [], [SomeModule])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      assert SomeModule in info.potential_modules
    end

    test "transitive private chain: pub -> priv B -> priv C" do
      fns = [
        pub(:foo, 0, [{nil, :b, 0}]),
        priv(:b, 0, [{nil, :c, 0}, {String, :foo, 1}]),
        priv(:c, 0, [{Enum, :map, 2}])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      refute {:TestModule, :b, 0} in info.calls
      refute {:TestModule, :c, 0} in info.calls
      assert {String, :foo, 1} in info.calls
      assert {Enum, :map, 2} in info.calls
    end

    test "cyclic private calls terminate without infinite loop" do
      # pub -> A -> B -> A (cycle)
      fns = [
        pub(:foo, 0, [{nil, :a, 0}]),
        priv(:a, 0, [{nil, :b, 0}]),
        priv(:b, 0, [{nil, :a, 0}])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      refute {:TestModule, :a, 0} in info.calls
      refute {:TestModule, :b, 0} in info.calls
    end

    test "diamond: two private helpers sharing a deeper private dependency" do
      # foo -> h1, h2; h1 -> deep; h2 -> deep
      fns = [
        pub(:foo, 0, [{nil, :h1, 0}, {nil, :h2, 0}]),
        priv(:h1, 0, [{nil, :deep, 0}, {String, :a, 1}]),
        priv(:h2, 0, [{nil, :deep, 0}, {String, :b, 1}]),
        priv(:deep, 0, [{Enum, :map, 2}])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      assert {Enum, :map, 2} in info.calls
    end

    test "expanded calls are deduplicated" do
      # h1 and h2 both call String.upcase/1
      fns = [
        pub(:foo, 0, [{nil, :h1, 0}, {nil, :h2, 0}]),
        priv(:h1, 0, [{String, :upcase, 1}]),
        priv(:h2, 0, [{String, :upcase, 1}])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      assert Enum.count(info.calls, &(&1 == {String, :upcase, 1})) == 1
    end

    test "expanded potential_modules are deduplicated" do
      fns = [
        pub(:foo, 0, [{nil, :h1, 0}, {nil, :h2, 0}]),
        priv(:h1, 0, [], [SomeModule]),
        priv(:h2, 0, [], [SomeModule])
      ]

      info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      assert Enum.count(info.potential_modules, &(&1 == SomeModule)) == 1
    end

    test "local call to a public function is not expanded (kept as-is)" do
      # local call to another public function should not pull that public function's privates
      fns = [
        pub(:foo, 0, [{nil, :bar, 0}]),
        pub(:bar, 0, [{nil, :secret, 0}]),
        priv(:secret, 0, [{Enum, :map, 2}])
      ]

      foo_info = PrivatesResolver.resolve(build_info(fns)) |> find_pub(:foo)
      # foo calls bar (public), bar calls secret (private of bar's scope).
      # foo should NOT expand secret because foo does not call secret — bar does.
      refute {Enum, :map, 2} in foo_info.calls
    end

    test "multiple public functions are each expanded independently" do
      fns = [
        pub(:foo, 0, [{nil, :shared, 0}]),
        pub(:bar, 1, []),
        priv(:shared, 0, [{Kernel, :+, 2}])
      ]

      result = PrivatesResolver.resolve(build_info(fns))
      foo_info = find_pub(result, :foo)
      bar_info = find_pub(result, :bar, 1)

      assert {Kernel, :+, 2} in foo_info.calls
      refute {Kernel, :+, 2} in bar_info.calls
    end
  end

  describe "analyze/1 - private function called_by" do
    test "directly called private function lists the public caller" do
      fns = [pub(:foo, 0, [{nil, :helper, 0}]), pub(:bar, 0, []), priv(:helper, 0)]
      result = PrivatesResolver.resolve(build_info(fns))
      assert {:foo, 0} in find_priv(result, :helper)
      refute {:bar, 0} in find_priv(result, :helper)
    end

    test "transitively called private function lists the public caller" do
      # foo -> b -> c; only foo is public
      fns = [
        pub(:foo, 0, [{nil, :b, 0}]),
        priv(:b, 0, [{nil, :c, 0}]),
        priv(:c, 0, [])
      ]

      result = PrivatesResolver.resolve(build_info(fns))
      assert {:foo, 0} in find_priv(result, :b)
      assert {:foo, 0} in find_priv(result, :c)
    end

    test "private function called by multiple public functions lists all of them" do
      fns = [
        pub(:foo, 0, [{nil, :shared, 0}]),
        pub(:bar, 0, [{nil, :shared, 0}]),
        priv(:shared, 0, [])
      ]

      result = PrivatesResolver.resolve(build_info(fns))
      assert {:foo, 0} in find_priv(result, :shared)
      assert {:bar, 0} in find_priv(result, :shared)
    end

    test "private function called transitively by multiple public functions lists all" do
      # foo -> helper -> deep; bar -> deep (directly)
      fns = [
        pub(:foo, 0, [{nil, :helper, 0}]),
        pub(:bar, 0, [{nil, :deep, 0}]),
        priv(:helper, 0, [{nil, :deep, 0}]),
        priv(:deep, 0, [])
      ]

      result = PrivatesResolver.resolve(build_info(fns))
      assert {:foo, 0} in find_priv(result, :deep)
      assert {:bar, 0} in find_priv(result, :deep)
    end

    test "uncalled private function has empty called_by list" do
      fns = [pub(:foo, 0, []), priv(:unused, 0, [])]
      result = PrivatesResolver.resolve(build_info(fns))
      assert find_priv(result, :unused) == []
    end

    test "private function called only by another private has empty called_by list" do
      # only public foo exists, and it doesn't call a or b
      fns = [
        pub(:foo, 0, []),
        priv(:a, 0, [{nil, :b, 0}]),
        priv(:b, 0, [])
      ]

      result = PrivatesResolver.resolve(build_info(fns))
      assert find_priv(result, :a) == []
      assert find_priv(result, :b) == []
    end
  end
end
