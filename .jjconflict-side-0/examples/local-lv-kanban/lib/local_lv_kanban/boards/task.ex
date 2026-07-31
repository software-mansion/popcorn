defmodule LocalLvKanban.Boards.Task do
  use Ecto.Schema
  import Ecto.Changeset

  alias LocalLvKanban.Boards.Column

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "tasks" do
    field :text, :string
    field :description, :string, default: ""
    field :position, :string
    belongs_to :column, Column
    timestamps()
  end

  def changeset(task, attrs) do
    task
    |> cast(attrs, [:text, :description, :position, :column_id])
    |> validate_required([:text, :position, :column_id])
    |> validate_length(:text, min: 1, max: 255)
    |> validate_length(:description, max: 2000)
    # If the column is deleted concurrently between add_task's board_column check
    # and this insert, degrade the FK violation to {:error, changeset} (→ :error
    # → rollback) instead of raising Ecto.ConstraintError.
    |> foreign_key_constraint(:column_id)
  end
end
