defmodule Phoenix.Template.Engine do
  @moduledoc """
  Specifies the API for adding custom template engines into Phoenix.

  Engines must implement the `compile/2` function, that receives
  the template file and the template name (usually used as the function
  name of the template) and outputs the template quoted expression:

      def compile(template_path, template_name)

  See `Phoenix.Template.EExEngine` for an example engine implementation.
  """

  @callback compile(template_path :: binary, template_name :: binary) :: Macro.t()
end
