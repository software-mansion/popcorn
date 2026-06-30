defmodule LocalLvKanban.Boards.Board do
  use Ecto.Schema
  import Ecto.Changeset

  alias LocalLvKanban.Boards.Column

  @primary_key {:id, :binary_id, autogenerate: true}
  @foreign_key_type :binary_id
  schema "boards" do
    field :name, :string
    has_many :columns, Column, preload_order: [asc: :position, asc: :id]
    timestamps()
  end

  def changeset(board, attrs) do
    board
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> validate_length(:name, min: 1, max: 255)
  end
end
