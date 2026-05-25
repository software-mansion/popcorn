defmodule Mix.Tasks.Popdoc.Build do
  use Mix.Task

  @shortdoc "Builds the Popdoc Popcorn runtime scaffold"

  @project_root Path.expand("../../..", __DIR__)
  @wasm_dir Path.join(@project_root, "wasm")
  @js_dir Path.join(@project_root, "js")

  @impl true
  def run(_args) do
    mix = find!("mix")
    node = find!("node")
    pnpm = find!("pnpm")

    ensure_wasm_deps!(mix)
    ensure_js_deps!(pnpm)

    step("Building wasm bundle", fn ->
      cmd!(mix, ["popcorn.cook", "--out-dir", "out"], @wasm_dir)
    end)

    step("Bundling JS runtime assets", fn ->
      cmd!(node, ["build.mjs"], @js_dir)
    end)

    Mix.shell().info("Done.")
  end

  defp ensure_wasm_deps!(mix) do
    deps_dir = Path.join(@wasm_dir, "deps")

    if File.dir?(deps_dir) do
      :ok
    else
      step("Fetching wasm deps", fn ->
        cmd!(mix, ["deps.get"], @wasm_dir)
      end)
    end
  end

  defp ensure_js_deps!(pnpm) do
    node_modules_dir = Path.join(@js_dir, "node_modules")
    pnpm_store_dir = Path.join(node_modules_dir, ".pnpm")

    if File.dir?(pnpm_store_dir) do
      :ok
    else
      step("Installing JS deps", fn ->
        # CI=true so pnpm won't prompt to purge an incompatible node_modules
        # dir (System.cmd has no TTY, so the prompt aborts the install).
        cmd!(pnpm, ["install"], @js_dir, [{"CI", "true"}])
      end)
    end
  end

  defp step(label, fun) do
    Mix.shell().info("==> #{label}...")
    fun.()
  end

  defp cmd!(exe, args, dir, env \\ []) do
    case System.cmd(exe, args, cd: dir, env: env, into: IO.stream(), stderr_to_stdout: true) do
      {_, 0} -> :ok
      {_, code} -> Mix.raise("`#{exe} #{Enum.join(args, " ")}` failed (exit #{code})")
    end
  end

  defp find!(name) do
    System.find_executable(name) || Mix.raise("#{name} not found in PATH.")
  end
end
