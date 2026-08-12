defmodule LocalLvKanban.Release do
  @moduledoc """
  Used for executing DB release tasks when run in production, without Mix
  installed.
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

  # Not part of the phx.gen.release template: runs priv/repo/seeds.exs, which
  # is idempotent, so this is safe on every deploy.
  def seed do
    load_app()

    for repo <- repos() do
      {:ok, _, _} =
        Ecto.Migrator.with_repo(repo, fn _repo ->
          seeds = Path.join([Application.app_dir(@app, "priv"), "repo", "seeds.exs"])
          if File.exists?(seeds), do: Code.eval_file(seeds)
        end)
    end
  end

  defp repos do
    Application.fetch_env!(@app, :ecto_repos)
  end

  defp load_app do
    Application.ensure_all_started(:ssl)
    Application.ensure_loaded(@app)
  end
end
