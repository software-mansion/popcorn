defmodule LocalLiveView.MirrorToken do
  @moduledoc """
  Handles signing and verifying mirror tokens for LocalLiveView.

  ## Usage

  Sign a mirror token:

      token = LocalLiveView.MirrorToken.sign(MyAppWeb.Endpoint, "MyView", "stable-id-123")

  Verify a mirror token:

      case LocalLiveView.MirrorToken.verify(MyAppWeb.Endpoint, token) do
        {:ok, %{id: id, view: view}} -> {:ok, id, view}
        {:error, reason} -> {:error, reason}
      end
  """

  @salt "llv:mirror"

  def sign(endpoint, view, id) when is_atom(endpoint) and is_binary(view) and is_binary(id) do
    Phoenix.Token.sign(endpoint, @salt, %{id: id, view: view})
  end

  def verify(endpoint, token, opts \\ []) when is_atom(endpoint) and is_binary(token) do
    max_age = Keyword.get(opts, :max_age, :infinity)
    Phoenix.Token.verify(endpoint, @salt, token, max_age: max_age)
  end
end
