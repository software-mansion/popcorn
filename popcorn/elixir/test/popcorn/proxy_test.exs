defmodule Popcorn.ProxyTest do
  use ExUnit.Case, async: true

  describe "startup" do
    test "ignores native VM" do
      assert Popcorn.Proxy.start_link() == :ignore
      assert Popcorn.Proxy.start_link(name: :host_proxy) == :ignore
      assert start_supervised({Popcorn.Proxy, name: :host_proxy}) == {:ok, :undefined}
      assert Process.whereis(:popcorn_proxy) == nil
      assert Process.whereis(:host_proxy) == nil

      assert Supervisor.which_children(Popcorn.Supervisor) == [
               {Popcorn.Proxy, :undefined, :worker, [Popcorn.Proxy]}
             ]
    end
  end
end
