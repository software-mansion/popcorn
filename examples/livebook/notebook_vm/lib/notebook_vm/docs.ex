defmodule NotebookVM.Docs do
  @moduledoc """
  Loads Elixir's and Erlang's docs, for Livebook's code completion and docs.

  Popcorn's package leaves them out, so Livebook's `mix cook` packs them in a
  tar of their own, with a chunk file for each module, in its application's
  `doc/chunks` directory. That's where `Code.fetch_docs/1` looks for the docs
  that a module's `.beam` file doesn't have. The VM boots without them, and
  loads them after.
  """

  require Logger

  # Next to the VM's package, see public/index.html
  @url "/notebook_vm/docs.tar"

  def load do
    with {:ok, %{status: 200, body: tar}} <- Popcorn.Fetch.request(%{method: "GET", url: @url}),
         :ok <- :erl_tar.extract({:binary, tar}, cwd: ~c"/") do
      :ok
    else
      error -> Logger.warning("Loading Elixir's and Erlang's docs failed: #{inspect(error)}")
    end
  end
end
