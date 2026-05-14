defmodule Treeshake.CallGraphTest do
  use ExUnit.Case, async: true

  @moduletag :treeshake

  setup_all do
    graph =
      Treeshake.config(project: "test/fixtures/treeshake/demo_app", include_stdlibs: true)
      |> Treeshake.build_module_index()
      |> Treeshake.build_call_graph()
      |> then(& &1.call_graph)

    if System.get_env("RESNAPSHOT") do
      graph_bin = :erlang.term_to_binary(graph, [:deterministic])
      File.write!("test/fixtures/treeshake/call_graph.bin", graph_bin)
    end

    {:ok, graph: graph}
  end

  # ---- graph keys and reachability ----

  describe "build_call_graph/1 - graph keys and reachability" do
    test "entry point is included as a key", %{graph: graph} do
      assert Map.has_key?(graph, {DemoApp.Application, :start, 2})
    end

    test "function reachable from entry point is added as a key", %{graph: graph} do
      assert Map.has_key?(graph, {DemoApp.BehaviourImplDep, :print_hello, 0})
    end

    test "unreachable public function is not included in the graph", %{graph: graph} do
      # Worker.unused/1 is public but never called from any reachable path
      refute Map.has_key?(graph, {DemoApp.Worker, :unused, 1})
    end
  end

  # ---- call values and external references ----

  describe "build_call_graph/1 - call values" do
    test "private helper calls are expanded into the public function's call list", %{graph: graph} do
      # Worker.process/1 -> private upcase/1 -> String.upcase/1
      calls = graph[{DemoApp.Worker, :process, 1}]
      assert {String, :upcase, 1} in calls
    end

    test "call list for a reachable callee is also populated", %{graph: graph} do
      # BehaviourImplDep.print_hello/0 calls IO.puts/2 which is external
      calls = graph[{DemoApp.BehaviourImplDep, :print_hello, 0}]
      assert is_list(calls)
      assert Enum.any?(calls, fn {m, _, _} -> m == IO end)
    end

    test "call lists contain no duplicates", %{graph: graph} do
      for {_mfa, calls} <- graph do
        assert calls == Enum.uniq(calls)
      end
    end
  end

  # ---- behaviour edges ----

  describe "build_call_graph/1 - behaviour edges" do
    test "potential_module implementing a behaviour adds callback edges to the call list",
         %{graph: graph} do
      # Application.start/2 passes DemoApp.BehaviourImpl as an atom argument,
      # so it appears in potential_modules. DemoApp.BehaviourImpl implements
      # DemoApp.Behaviour which declares hello/0 as a callback.
      # => {DemoApp.BehaviourImpl, :hello, 0} must appear in start/2's call list.
      calls = graph[{DemoApp.Application, :start, 2}]
      assert {DemoApp.BehaviourImpl, :hello, 0} in calls
    end

    test "callback implementation is reachable via BFS through the behaviour edge",
         %{graph: graph} do
      assert Map.has_key?(graph, {DemoApp.BehaviourImpl, :hello, 0})
    end

    test "potential_module not implementing any behaviour adds no extra edges", %{graph: graph} do
      # BehaviourImpl.hello/0 only calls print_hello/0 — no extra behaviour edges
      calls = graph[{DemoApp.BehaviourImpl, :hello, 0}]
      assert calls == [{DemoApp.BehaviourImplDep, :print_hello, 0}]
    end
  end

  # ---- protocol edges ----

  describe "build_call_graph/1 - protocol edges" do
    test "protocol dispatch key is a graph node when protocol is called from reachable code",
         %{graph: graph} do
      # ProtocolUser.run/0 calls DemoApp.Formatter.format/1, so the protocol
      # dispatch key must appear as a graph node.
      assert Map.has_key?(graph, {DemoApp.Formatter, :format, 1})
    end

    test "implementation for a built-in type is reachable when protocol is called",
         %{graph: graph} do
      # Integer is always in referenced_modules (built-in type), so the Integer
      # implementation must be in the dispatch node's call list and be reachable.
      calls = graph[{DemoApp.Formatter, :format, 1}]
      assert {DemoApp.Formatter.Integer, :format, 1} in calls
      assert Map.has_key?(graph, {DemoApp.Formatter.Integer, :format, 1})
    end

    test "implementation for a referenced struct type is reachable",
         %{graph: graph} do
      # ProtocolUser.run/0 creates a %DemoApp.Widget{} struct literal, so
      # DemoApp.Widget appears in potential_modules and becomes a referenced module.
      # The Widget implementation must therefore be in the dispatch node and reachable.
      calls = graph[{DemoApp.Formatter, :format, 1}]
      assert {DemoApp.Formatter.DemoApp.Widget, :format, 1} in calls
      assert Map.has_key?(graph, {DemoApp.Formatter.DemoApp.Widget, :format, 1})
    end

    test "implementation for an unreferenced struct type is not reachable",
         %{graph: graph} do
      # DemoApp.Gadget is never mentioned in any reachable function, so its
      # Formatter implementation must not appear in the graph at all.
      refute Map.has_key?(graph, {DemoApp.Formatter.DemoApp.Gadget, :format, 1})
    end

    test "impl_for/1 dispatch node has only implementation nodes as calls (not further protocol calls)",
         %{graph: graph} do
      # impl_for/1 is skipped during BFS traversal so no further edges are
      # generated FROM it. Its call list only contains the per-implementation
      # impl_for/1 nodes added via the protocol chunk (not arbitrary sub-calls).
      calls = Map.get(graph, {DemoApp.Formatter, :impl_for, 1}, [])

      assert Enum.all?(calls, fn {m, f, a} ->
               f == :impl_for and a == 1 and m != DemoApp.Formatter
             end)
    end
  end

  # ---- BFS termination and cycle safety ----

  describe "build_call_graph/1 - BFS termination" do
    test "terminates and returns a map", %{graph: graph} do
      assert is_map(graph)
    end

    test "each reachable node appears as a key exactly once", %{graph: graph} do
      keys = Map.keys(graph)
      assert keys == Enum.uniq(keys)
    end
  end

  defguardp is_mfa(mfa)
            when is_tuple(mfa) and tuple_size(mfa) == 3 and is_atom(elem(mfa, 0)) and
                   is_atom(elem(mfa, 1)) and is_integer(elem(mfa, 2))

  test "graph structure is correct", %{graph: graph} do
    for entry <- graph do
      assert {mfa, calls} = entry
      assert is_mfa(mfa)
      assert is_list(calls)

      for call <- calls do
        assert is_mfa(call)
      end
    end
  end

  @tag :cg_fixture
  test "verify graph against fixture", %{graph: graph} do
    fixture =
      File.read!("test/fixtures/treeshake/call_graph.bin")
      |> :erlang.binary_to_term()

    assert [fixture_diff: %{}, graph_diff: %{}] == [
             fixture_diff: Treeshake.Utils.Graph.diff(fixture, graph),
             graph_diff: Treeshake.Utils.Graph.diff(graph, fixture)
           ]
  end
end
