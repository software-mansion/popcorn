defmodule Mix.Tasks.Popdoc.Server do
  @shortdoc "Starts a static file server with COOP/COEP headers for Popdoc docs."
  @moduledoc """
  #{@shortdoc}

  Serves files from the configured directory with the HTTP headers required by
  browsers to run WebAssembly with `SharedArrayBuffer`.

  This task mirrors the existing `mix popcorn.server` flow, but defaults to the
  generated ExDoc output directory used by Popdoc projects.

  The server runs as a separate Elixir process using `Mix.install` to fetch
  Bandit and Plug, so these dependencies don't need to be in your project's
  `mix.exs`.

  ## Options

    * `--port` - port to listen on (default: 4000)
    * `--dir` - directory to serve files from (default: "doc")

  ## Examples

      $ mix popdoc.server
      $ mix popdoc.server --port 8080 --dir doc
  """

  use Mix.Task

  @impl true
  def run(args) do
    %{dir: dir, port: port} = Popdoc.Server.parse_options!(args)

    elixir =
      System.find_executable("elixir") ||
        Mix.raise("elixir executable not found")

    elixir_args = [
      "--erl",
      "+Bi",
      "-e",
      Popdoc.Server.script(),
      "--",
      "--port",
      to_string(port),
      "--dir",
      dir
    ]

    {:spawn_executable, elixir}
    |> Port.open([
      :binary,
      :exit_status,
      :stderr_to_stdout,
      args: elixir_args
    ])
    |> forward_output()
  end

  defp forward_output(child) do
    receive do
      {^child, {:data, data}} ->
        IO.write(data)
        forward_output(child)

      {^child, {:exit_status, _status}} ->
        :ok
    end
  end
end
