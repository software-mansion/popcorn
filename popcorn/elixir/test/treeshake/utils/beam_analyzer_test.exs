defmodule Treeshake.Utils.BeamAnalyzerTest do
  use ExUnit.Case, async: true

  @moduletag :treeshake

  alias Treeshake.Utils.BeamAnalyzer

  @ebin "test/fixtures/treeshake/demo_app/_build/prod/lib/demo_app/ebin"

  defp analyze(module, dir \\ @ebin) do
    beam_path = Path.join(dir, "#{module}.beam")
    {:ok, module, core} = Treeshake.Utils.BeamReader.read_core(beam_path)
    BeamAnalyzer.analyze(module, core)
  end

  defp find_fun(functions, name, arity) do
    Enum.find(functions, &(&1.name == name and &1.arity == arity))
  end

  describe "read/2 - module metadata" do
    test "returns the module atom" do
      info = analyze(DemoApp.Worker)
      assert info.module == DemoApp.Worker
    end

    test "abstraction is nil for a plain module" do
      info = analyze(DemoApp.Worker)
      assert info.abstraction == nil
    end

    test "abstraction is {:protocol, callbacks} for a defprotocol module" do
      info = analyze(DemoApp.Formatter)
      assert match?({:protocol, _}, info.abstraction)
    end

    test "abstraction is nil for a defimpl module" do
      info = analyze(DemoApp.Formatter.Integer)
      assert info.abstraction == nil
    end

    test "abstraction is nil for a behaviour implementor (not itself a behaviour)" do
      info = analyze(DemoApp.BehaviourImpl)
      assert info.abstraction == nil
    end
  end

  describe "read/2 - public / private" do
    test "exported functions are marked public" do
      info = analyze(DemoApp.Worker)

      assert find_fun(info.functions, :process, 1).public == true
      assert find_fun(info.functions, :unused, 1).public == true
    end

    test "non-exported functions are marked private" do
      info = analyze(DemoApp.Worker)

      assert find_fun(info.functions, :upcase, 1).public == false
      assert find_fun(info.functions, :wrap, 1).public == false
    end

    test "all four functions of DemoApp.Worker are present" do
      info = analyze(DemoApp.Worker)
      names = Enum.map(info.functions, &{&1.name, &1.arity})

      assert {:process, 1} in names
      assert {:unused, 1} in names
      assert {:upcase, 1} in names
      assert {:wrap, 1} in names
    end
  end

  describe "read/2 - calls" do
    test "process/1 calls private helpers locally" do
      info = analyze(DemoApp.Worker)
      calls = find_fun(info.functions, :process, 1).calls

      assert {nil, :upcase, 1} in calls
      assert {nil, :wrap, 1} in calls
    end

    test "upcase/1 calls String.upcase/1 remotely" do
      info = analyze(DemoApp.Worker)
      calls = find_fun(info.functions, :upcase, 1).calls

      assert {String, :upcase, 1} in calls
    end

    test "BehaviourImpl.hello/0 calls BehaviourImplDep.print_hello/0 remotely" do
      info = analyze(DemoApp.BehaviourImpl)
      calls = find_fun(info.functions, :hello, 0).calls

      assert {DemoApp.BehaviourImplDep, :print_hello, 0} in calls
    end

    test "standalone atom literals go into potential_modules, not calls" do
      info = analyze(DemoApp.BehaviourImpl)
      hello = find_fun(info.functions, :hello, 0)

      # :ok is a standalone atom literal — not part of a call or MFA tuple
      assert :ok in hello.potential_modules
      refute :ok in hello.calls

      # The remote-call module is represented in calls, not in potential_modules
      assert {DemoApp.BehaviourImplDep, :print_hello, 0} in hello.calls
      refute DemoApp.BehaviourImplDep in hello.potential_modules
    end

    test "calls list has no duplicates" do
      info = analyze(DemoApp.Worker)

      for func <- info.functions do
        assert func.calls == Enum.uniq(func.calls),
               "duplicate calls in #{func.name}/#{func.arity}"
      end
    end
  end

  describe "read/2 - abstraction" do
    test "behaviour module has {:behaviour, callbacks}" do
      info = analyze(DemoApp.Behaviour)

      assert {:behaviour, cbs} = info.abstraction
      assert {:hello, 0} in cbs
    end

    test "protocol module has {:protocol, callbacks}" do
      info = analyze(DemoApp.Formatter)

      assert {:protocol, cbs} = info.abstraction
      assert {:format, 1} in cbs
    end

    test "protocol and behaviour abstractions are distinct tags" do
      proto_info = analyze(DemoApp.Formatter)
      beh_info = analyze(DemoApp.Behaviour)

      assert match?({:protocol, _}, proto_info.abstraction)
      assert match?({:behaviour, _}, beh_info.abstraction)
    end

    test "plain module has nil abstraction" do
      info = analyze(DemoApp.Worker)

      assert info.abstraction == nil
    end
  end

  describe "read/2 - implemented_protocols" do
    test "protocol implementation module has :protocol_impl" do
      info = analyze(DemoApp.Formatter.Integer)
      assert info.protocol_impl == {DemoApp.Formatter, Integer}
    end

    test ":protocol_impl is nil for a non-implementation module" do
      info = analyze(DemoApp.Worker)
      assert info.protocol_impl == nil
    end

    test ":protocol_impl is nil for the protocol definition module itself" do
      info = analyze(DemoApp.Formatter)
      assert info.protocol_impl == nil
    end
  end

  describe "read/2 - DemoApp.Formatter (protocol definition)" do
    test "full metadata shape" do
      info = analyze(DemoApp.Formatter)

      assert info.module == DemoApp.Formatter
      assert info.abstraction == {:protocol, [{:format, 1}]}
      assert info.behaviour_impls == []
      assert info.protocol_impl == nil
    end

    test "format/1 is a public function" do
      info = analyze(DemoApp.Formatter)
      format = find_fun(info.functions, :format, 1)

      assert format != nil
      assert format.public == true
    end
  end

  describe "read/2 - DemoApp.Formatter.Integer (protocol implementation)" do
    test "full metadata shape" do
      info = analyze(DemoApp.Formatter.Integer)

      assert info.module == DemoApp.Formatter.Integer
      assert info.abstraction == nil
      assert info.protocol_impl == {DemoApp.Formatter, Integer}
      assert info.behaviour_impls == []
    end

    test "format/1 is a public function" do
      info = analyze(DemoApp.Formatter.Integer)
      format = find_fun(info.functions, :format, 1)

      assert format != nil
      assert format.public == true
    end
  end

  describe "read/2 - potential_modules excludes pattern match and guard atoms" do
    test "function-head pattern atom is absent from potential_modules" do
      info = analyze(DemoApp.PatternMatcher)
      check = find_fun(info.functions, :check, 1)

      refute :head_pattern_only in check.potential_modules
    end

    test "function-head guard atom is absent from potential_modules" do
      info = analyze(DemoApp.PatternMatcher)
      guarded = find_fun(info.functions, :guarded, 1)

      refute :head_guard_only in guarded.potential_modules
    end

    test "case-clause pattern atom in function body is absent from potential_modules" do
      info = analyze(DemoApp.PatternMatcher)
      classify = find_fun(info.functions, :classify, 1)

      refute :case_pattern_only in classify.potential_modules
    end

    test "case-clause guard atom in function body is absent from potential_modules" do
      info = analyze(DemoApp.PatternMatcher)
      filter = find_fun(info.functions, :filter, 1)

      refute :case_guard_only in filter.potential_modules
    end

    test "atoms in clause bodies are still collected into potential_modules" do
      info = analyze(DemoApp.PatternMatcher)
      check = find_fun(info.functions, :check, 1)
      classify = find_fun(info.functions, :classify, 1)

      assert :head_result in check.potential_modules or :other_result in check.potential_modules

      assert :classified in classify.potential_modules or
               :unclassified in classify.potential_modules
    end
  end

  @tag :task
  test "hardcoded MFA tuple {Mod, :fun, arity} appears in calls" do
    info = analyze(Task, "test/fixtures/treeshake/ebin")
    child_spec = Enum.find(info.functions, &(&1.name == :child_spec))

    # Task.child_spec/1 contains the literal {Task, :start_link, [arg]} — arity 1
    assert {Task, :start_link, 1} in child_spec.calls

    # Task atom from `id: Task` goes into potential_modules, not calls
    assert Task in child_spec.potential_modules
    refute Task in child_spec.calls
  end

  test "task" do
    info = analyze(Task, "test/fixtures/treeshake/ebin")

    assert child_spec = Enum.find(info.functions, &(&1.name == :child_spec))

    assert Enum.find(child_spec.calls, fn
             {Task, :start_link, _args} -> true
             _other -> false
           end)
  end
end
