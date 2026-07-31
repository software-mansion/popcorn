defmodule LocalLvKanban.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production without Mix
  installed, e.g. `bin/local_lv_kanban eval "LocalLvKanban.Release.migrate()"`.
  """
  @app :local_lv_kanban

  def migrate do
    load_app()

    for repo <- repos() do
      {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :up, all: true))
    end
  end

  def rollback(repo, version) do
    load_app()
    {:ok, _, _} = Ecto.Migrator.with_repo(repo, &Ecto.Migrator.run(&1, :down, to: version))
  end

  # Seeds are idempotent (see priv/repo/seeds.exs), so this is safe to run on
  # every boot.
  def seed do
    load_app()

    for repo <- repos() do
      {:ok, _, _} =
        Ecto.Migrator.with_repo(repo, fn repo ->
          seeds = Path.join([:code.priv_dir(@app), "repo", "seeds.exs"])
          if File.exists?(seeds), do: Code.eval_file(seeds)
          repo
        end)
    end
  end

  defp repos do
    Application.fetch_env!(@app, :ecto_repos)
  end

  defp load_app do
    Application.load(@app)
  end
end
