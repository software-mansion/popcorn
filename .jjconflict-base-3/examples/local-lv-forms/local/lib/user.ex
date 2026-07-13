defmodule FormDemoLocal.User do
  use Ecto.Schema
  import Ecto.Changeset

  @primary_key false
  embedded_schema do
    field :username, :string
    field :email, :string
  end

  def changeset(user, attrs, existing_users \\ []) do
    user
    |> cast(attrs, [:username, :email])
    |> validate_required([:username, :email])
    |> validate_length(:username, min: 4)
    |> validate_email(:email)
    |> validate_unique(:username, existing_users)
    |> validate_unique(:email, existing_users)
  end

  defp validate_unique(changeset, field, existing_users) do
    validate_change(changeset, field, fn ^field, value ->
      if Enum.any?(existing_users, fn user -> Map.get(user, field) == value end) do
        [{field, "is already in use"}]
      else
        []
      end
    end)
  end

  defp validate_email(changeset, field) do
    validate_change(changeset, field, fn ^field, value ->
      if valid_email?(value), do: [], else: [{field, "must have an email format"}]
    end)
  end

  defp valid_email?(value) do
    case String.split(value, "@") do
      [name, domain] -> name != "" and String.contains?(domain, ".")
      _ -> false
    end
  end
end
