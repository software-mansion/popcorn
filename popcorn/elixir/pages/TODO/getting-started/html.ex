defmodule Counters.HTML do
  def by_id(id) do
    """
    ({ id }) => {
      return document.getElementById(id)l
    }
    """
    |> Wasm.run_js!(%{id: id})
  end
end
