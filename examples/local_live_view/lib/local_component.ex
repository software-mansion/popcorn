defmodule LocalComponent do

  @doc false
  defmacro __using__(opts \\ []) do
#    conditional =
#      if __CALLER__.module != Phoenix.LiveView.Helpers do
#        quote do: import(Phoenix.LiveView.Helpers)
#      end

    imports =
      quote bind_quoted: [opts: opts] do
#        import Kernel, except: [def: 2, defp: 2]
#        import Phoenix.Component
#        import Phoenix.Component.Declarative
#        require Phoenix.Template
#
#        for {prefix_match, value} <- Phoenix.Component.Declarative.__setup__(__MODULE__, opts) do
#          @doc false
#          def __global__?(unquote(prefix_match)), do: unquote(value)
#        end
        import LocalComponent
      end

    [imports]
  end
  
  @doc type: :macro
  defmacro sigil_H({:<<>>, meta, [expr]}, modifiers)
           when modifiers == [] or modifiers == ~c"noformat" do
    if not Macro.Env.has_var?(__CALLER__, {:assigns, nil}) do
      raise "~H requires a variable named \"assigns\" to exist and be set to a map"
    end

#    TODO the following is a proper way of preparing a %Phoenix.LiveView.Rendered{} struct but for now LocalComponent will return a string
    options = [
      engine: Phoenix.LiveView.TagEngine,
      file: __CALLER__.file,
      line: __CALLER__.line + 1,
      caller: __CALLER__,
      indentation: meta[:indentation] || 0,
      source: expr,
      tag_handler: Phoenix.LiveView.HTMLEngine
    ]

    EEx.compile_string(expr, options)
  end
end
