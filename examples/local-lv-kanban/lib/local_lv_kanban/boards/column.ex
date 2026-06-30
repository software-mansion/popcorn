defmodule LocalLvKanban.Boards.Column do
  use Ecto.Schema
  import Ecto.Changeset

  alias LocalLvKanban.Boards.{Board, Task}

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "columns" do
    field :name, :string
    field :position, :integer
    belongs_to :board, Board
    has_many :tasks, Task, preload_order: [asc: :position]
    timestamps()
  end

  def changeset(column, attrs) do
    column
    |> cast(attrs, [:name, :position, :board_id])
    |> validate_required([:name, :position, :board_id])
    |> validate_length(:name, min: 1, max: 255)
  end
end
