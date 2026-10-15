defmodule Popcorn.JSTest do
  use ExUnit.Case

  require Popcorn.JS
  alias Popcorn.JS
  alias Popcorn.JS.FakeBridge

  describe "available?/0" do
    test "is false on the host, where the runtime module is absent" do
      refute JS.available?()
    end
  end

  describe "await_ready/1" do
    setup :use_fake_bridge

    test "waits until JS is ready" do
      assert :ok = JS.await_ready(timeout: 10)
      assert [timeout: 10] = Process.get(:await_ready_opts)
      assert {:error, :timeout} = JS.await_ready(timeout: 0)
      assert_raise FunctionClauseError, fn -> apply(JS, :await_ready, [:infinity]) end
    end
  end

  describe "is_message/1" do
    test "matches messages from JS" do
      assert JS.is_message({:wasm, %{"a" => 1}})
    end

    test "rejects other terms" do
      refute JS.is_message({:wasm, :payload, :meta})
      refute JS.is_message({:other, :payload, :meta})
      refute JS.is_message(:wasm)
      refute JS.is_message("wasm")
    end

    test "is usable in a guard" do
      assert match?(msg when JS.is_message(msg), {:wasm, :payload})
    end
  end

  describe "run_js/3" do
    setup :use_fake_bridge

    test "wraps the bridge result in :ok and forwards args and opts" do
      assert {:ok, %{"args" => %{n: 1}, "opts" => [timeout: 10]}} =
               JS.run_js("f", %{n: 1}, timeout: 10)
    end

    test "translates a bridge timeout" do
      assert JS.run_js("f", %{mode: :timeout}) == {:error, :timeout}
    end

    test "translates a JS exception" do
      assert JS.run_js("f", %{mode: :js_error}) == {:error, {:js, "TypeError: x"}}
    end

    test "lets other errors propagate untranslated" do
      assert_raise ArgumentError, fn -> JS.run_js("f", %{mode: :badarg}) end
      assert_raise RuntimeError, "boom", fn -> JS.run_js("f", %{mode: :raise}) end
    end

    test "rejects invalid arguments loudly" do
      assert_raise FunctionClauseError, fn -> JS.run_js("f", :not_a_map) end
      assert_raise FunctionClauseError, fn -> JS.run_js(:not_a_binary, %{}) end
    end
  end

  describe "run_js!/3" do
    setup :use_fake_bridge

    test "returns the result directly" do
      assert %{"args" => %{n: 1}} = JS.run_js!("f", %{n: 1})
    end

    test "raises Popcorn.JS.Error on a timeout" do
      assert_raise JS.Error, "JS didn't reply in time", fn ->
        JS.run_js!("f", %{mode: :timeout})
      end
    end

    test "raises Popcorn.JS.Error on a JS exception" do
      assert_raise JS.Error, "JS raised: 'TypeError: x'", fn ->
        JS.run_js!("f", %{mode: :js_error})
      end
    end

    test "carries the reason run_js/3 would have returned" do
      e = assert_raise JS.Error, fn -> JS.run_js!("f", %{mode: :js_error}) end
      assert e.reason == {:js, "TypeError: x"}
      assert JS.run_js("f", %{mode: :js_error}) == {:error, e.reason}
    end

    test "is rescuable by name" do
      result =
        try do
          JS.run_js!("f", %{mode: :timeout})
        rescue
          e in JS.Error -> e.reason
        end

      assert result == :timeout
    end

    test "lets other errors propagate untranslated" do
      assert_raise ArgumentError, fn -> JS.run_js!("f", %{mode: :badarg}) end
      assert_raise RuntimeError, "boom", fn -> JS.run_js!("f", %{mode: :raise}) end
    end
  end

  describe "Popcorn.JS.Error" do
    test "inspects a non-binary JS reason" do
      assert Exception.message(%JS.Error{reason: {:js, %{"a" => 1}}}) ==
               ~s(JS raised: '%{"a" => 1}')
    end
  end

  describe "send/1" do
    setup :use_fake_bridge

    test "delegates to the bridge" do
      assert JS.send(%{hello: "js"}) == :ok
      assert Process.get(:sent) == %{hello: "js"}
    end
  end

  describe "host fallback" do
    test "uses a no-op mock" do
      assert :ok = JS.await_ready(timeout: 10)
      assert {:ok, nil} = JS.run_js("f", %{n: 1})
      assert is_nil(JS.run_js!("f", %{n: 1}))
      assert :ok = JS.send(%{hello: "js"})
    end
  end

  defp use_fake_bridge(_context) do
    Application.put_env(:popcorn, :wasm_bridge, FakeBridge)
    on_exit(fn -> Application.delete_env(:popcorn, :wasm_bridge) end)
  end
end
