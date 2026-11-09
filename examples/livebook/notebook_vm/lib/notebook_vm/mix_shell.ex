defmodule NotebookVM.MixShell do
  @moduledoc """
  Mix's shell in the VM. It compiles rebar3 dependencies with
  `NotebookVM.Rebar` when Mix runs rebar3, as the browser can't run OS
  commands.
  """
  @behaviour Mix.Shell

  @impl true
  defdelegate info(message), to: Mix.Shell.IO

  @impl true
  defdelegate error(message), to: Mix.Shell.IO

  @impl true
  defdelegate prompt(message), to: Mix.Shell.IO

  @impl true
  defdelegate yes?(message), to: Mix.Shell.IO

  @impl true
  defdelegate yes?(message, opts), to: Mix.Shell.IO

  @impl true
  defdelegate print_app(), to: Mix.Shell.IO

  @impl true
  def cmd(command), do: cmd(command, [])

  @impl true
  def cmd({exec, ["bare", "compile", "--paths", paths]} = command, opts) do
    if exec == System.get_env("MIX_REBAR3") do
      NotebookVM.Rebar.compile(Keyword.fetch!(opts, :cd), Map.new(opts[:env] || []), paths)
    else
      unsupported(command)
    end
  end

  def cmd(command, _opts), do: unsupported(command)

  defp unsupported(command) do
    error("Can't run #{inspect(command)}: the browser can't run OS commands")
    1
  end
end
