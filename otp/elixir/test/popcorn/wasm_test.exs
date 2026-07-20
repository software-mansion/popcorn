defmodule Popcorn.WasmTest do
  use ExUnit.Case, async: true

  require Popcorn.Wasm
  alias Popcorn.Wasm
  alias Popcorn.Wasm.FakeBridge

  setup do
    Application.put_env(:popcorn_otp, :wasm_bridge, FakeBridge)
    on_exit(fn -> Application.delete_env(:popcorn_otp, :wasm_bridge) end)
  end

  describe "available?/0" do
    test "is false on the host, where the runtime module is absent" do
      refute Wasm.available?()
    end
  end

  describe "is_message/1" do
    test "matches messages from JS" do
      assert Wasm.is_message({:wasm, %{"a" => 1}})
    end

    test "rejects other terms" do
      refute Wasm.is_message({:wasm, :payload, :meta})
      refute Wasm.is_message({:other, :payload, :meta})
      refute Wasm.is_message(:wasm)
      refute Wasm.is_message("wasm")
    end

    test "is usable in a guard" do
      assert match?(msg when Wasm.is_message(msg), {:wasm, :payload})
    end
  end

  describe "run_js/3" do
    test "wraps the bridge result in :ok and forwards args and opts" do
      assert {:ok, %{"args" => %{n: 1}, "opts" => [timeout: 10]}} =
               Wasm.run_js("f", %{n: 1}, timeout: 10)
    end

    test "translates a bridge timeout" do
      assert Wasm.run_js("f", %{mode: :timeout}) == {:error, :timeout}
    end

    test "translates a JS exception" do
      assert Wasm.run_js("f", %{mode: :js_error}) == {:error, {:js, "TypeError: x"}}
    end

    test "lets other errors propagate untranslated" do
      assert_raise ArgumentError, fn -> Wasm.run_js("f", %{mode: :badarg}) end
      assert_raise RuntimeError, "boom", fn -> Wasm.run_js("f", %{mode: :raise}) end
    end

    test "rejects invalid arguments loudly" do
      assert_raise FunctionClauseError, fn -> Wasm.run_js("f", :not_a_map) end
      assert_raise FunctionClauseError, fn -> Wasm.run_js(:not_a_binary, %{}) end
    end
  end

  describe "run_js!/3" do
    test "returns the result directly" do
      assert %{"args" => %{n: 1}} = Wasm.run_js!("f", %{n: 1})
    end

    test "raises Popcorn.Wasm.Error on a timeout" do
      assert_raise Wasm.Error, "JS didn't reply in time", fn ->
        Wasm.run_js!("f", %{mode: :timeout})
      end
    end

    test "raises Popcorn.Wasm.Error on a JS exception" do
      assert_raise Wasm.Error, "JS raised: 'TypeError: x'", fn ->
        Wasm.run_js!("f", %{mode: :js_error})
      end
    end

    test "carries the reason run_js/3 would have returned" do
      e = assert_raise Wasm.Error, fn -> Wasm.run_js!("f", %{mode: :js_error}) end
      assert e.reason == {:js, "TypeError: x"}
      assert Wasm.run_js("f", %{mode: :js_error}) == {:error, e.reason}
    end

    test "is rescuable by name" do
      result =
        try do
          Wasm.run_js!("f", %{mode: :timeout})
        rescue
          e in Wasm.Error -> e.reason
        end

      assert result == :timeout
    end

    test "lets other errors propagate untranslated" do
      assert_raise ArgumentError, fn -> Wasm.run_js!("f", %{mode: :badarg}) end
      assert_raise RuntimeError, "boom", fn -> Wasm.run_js!("f", %{mode: :raise}) end
    end
  end

  describe "Popcorn.Wasm.Error" do
    test "inspects a non-binary JS reason" do
      assert Exception.message(%Wasm.Error{reason: {:js, %{"a" => 1}}}) ==
               ~s(JS raised: '%{"a" => 1}')
    end
  end

  describe "send/1" do
    test "delegates to the bridge" do
      assert Wasm.send(%{hello: "js"}) == :ok
      assert Process.get(:sent) == %{hello: "js"}
    end
  end
end
