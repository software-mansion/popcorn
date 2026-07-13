defmodule TreeshakeTest do
  use ExUnit.Case, async: true

  import AsyncTest

  @moduletag :treeshake

  @fixture "test/fixtures/treeshake/demo_app"

  @moduletag :tmp_dir

  defp treeshake(opts) do
    Treeshake.run([project: @fixture, include_stdlibs: true] ++ opts)
  end

  setup_all do
    {:ok, stats_agent} = Agent.start_link(fn -> nil end)
    %{stats_agent: stats_agent}
  end

  defp get_stats(ctx) do
    Agent.get_and_update(
      ctx.stats_agent,
      fn
        {:stats, stats} = state ->
          {stats, state}

        nil ->
          tmp_dir = "tmp/treeshake_test"
          out_dir = Path.join(tmp_dir, "out")
          File.mkdir_p!(tmp_dir)
          stats = treeshake(tmp_dir: tmp_dir, output_dir: out_dir)
          {stats, {:stats, stats}}
      end,
      :infinity
    )
  end

  @tag :module_removal
  async_test "module-level removal", ctx do
    stats = get_stats(ctx)
    refute DemoApp.Application in stats.modules_removed
    refute DemoApp.Worker in stats.modules_removed
    assert DemoApp.DeadModule in stats.modules_removed
    assert DemoApp.AnotherDead in stats.modules_removed
    assert DemoApp in stats.modules_removed

    surviving =
      stats.output_dir
      |> File.ls!()
      |> Enum.flat_map(fn
        "Elixir.DemoApp." <> app_module -> [app_module]
        _other -> []
      end)
      |> Enum.sort()

    assert ~w|Application.beam Behaviour.beam BehaviourImpl.beam BehaviourImplDep.beam Formatter.DemoApp.Widget.beam Formatter.Integer.beam Formatter.beam ProtocolUser.beam Worker.beam| =
             surviving
  end

  describe "function-level removal" do
    async_test "removes unused/1 from DemoApp.Worker (non-dead module)", ctx do
      stats = get_stats(ctx)
      assert {:unused, 1} in stats.modules_shaked[DemoApp.Worker]
    end

    async_test "unused/1 is not callable after tree-shaking", ctx do
      stats = get_stats(ctx)
      beam_path = Path.join(stats.output_dir, "Elixir.DemoApp.Worker.beam")
      {:ok, binary} = File.read(beam_path)

      assert {:module, DemoApp.Worker} =
               :code.load_binary(DemoApp.Worker, String.to_charlist(beam_path), binary)

      on_exit(fn ->
        :code.purge(DemoApp.Worker)
        :code.delete(DemoApp.Worker)
      end)

      refute function_exported?(DemoApp.Worker, :unused, 1)
    end
  end

  async_test "dry run", %{tmp_dir: tmp_dir} do
    ebin = Path.join([@fixture, "_build", "prod", "lib", "demo_app", "ebin"])
    before_files = ebin |> File.ls!() |> Enum.sort()
    out_dir = Path.join(tmp_dir, "out")

    stats = treeshake(tmp_dir: tmp_dir, output_dir: out_dir, dry_run: true)

    assert DemoApp.DeadModule in stats.modules_removed
    assert DemoApp.AnotherDead in stats.modules_removed

    refute File.exists?(out_dir)

    assert before_files == ebin |> File.ls!() |> Enum.sort()
  end

  @tag :behaviour
  async_test "behaviour", ctx do
    stats = get_stats(ctx)
    refute DemoApp.Behaviour in stats.modules_removed
    refute DemoApp.BehaviourImpl in stats.modules_removed
    refute DemoApp.BehaviourImplDep in stats.modules_removed
  end

  @tag :correctness
  async_test "surviving modules are callable after tree-shaking", %{tmp_dir: tmp_dir} do
    output_dir = Path.join(tmp_dir, "out")

    treeshake(
      tmp_dir: tmp_dir,
      output_dir: output_dir,
      stub_removed_modules: true
    )

    erl = Path.join([:code.root_dir() |> to_string(), "bin", "erl"])

    {output, exit_code} =
      System.cmd(
        erl,
        ~w|-noshell -noinput -pa #{output_dir} -run treeshake_helper start demo_app|,
        stderr_to_stdout: true
      )

    assert exit_code == 0,
           "erl subprocess failed (exit #{exit_code}): missing module or wrong result\n#{output}"
  end
end
