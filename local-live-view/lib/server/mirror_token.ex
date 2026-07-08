defmodule LocalLiveView.MirrorToken do
  @moduledoc false

  @salt "llv:mirror"

  def sign(endpoint, view, id) when is_atom(endpoint) and is_binary(view) and is_binary(id) do
    Phoenix.Token.sign(endpoint, @salt, %{id: id, view: view})
  end

  def verify(endpoint, token, opts \\ []) when is_atom(endpoint) and is_binary(token) do
    max_age = Keyword.get(opts, :max_age, :infinity)
    Phoenix.Token.verify(endpoint, @salt, token, max_age: max_age)
  end
end
