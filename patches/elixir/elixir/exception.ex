defmodule Exception do
  @compile {:popcorn_patch_private, format_application: 1}
  def format_application(_module) do
    # Patch reason: :application.get_application uses :ets.match,
    # not implemented in AtomVM

    # case :application.get_application(module) do
    #   {:ok, app} -> "(" <> Atom.to_string(app) <> ") "
    #   :undefined -> ""
    # end
    ""
  end
end
