defmodule LocalLvKanban.Repo.Migrations.CreateKanban do
  use Ecto.Migration

  def change do
    create table(:boards, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :name, :string, null: false, default: "Untitled board"
      timestamps()
    end

    create table(:columns, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :board_id, references(:boards, type: :binary_id, on_delete: :delete_all), null: false
      add :name, :string, null: false
      add :position, :integer, null: false
      timestamps()
    end

    create index(:columns, [:board_id])

    create table(:tasks, primary_key: false) do
      add :id, :binary_id, primary_key: true
      add :column_id, references(:columns, type: :binary_id, on_delete: :delete_all), null: false
      add :text, :string, null: false
      add :description, :text, null: false, default: ""
      add :position, :text, null: false
      timestamps()
    end

    create index(:tasks, [:column_id])
  end
end
