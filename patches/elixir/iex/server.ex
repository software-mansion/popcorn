defmodule IEx.Server do
  # Patch reason: String.replace/3 needs regex to work - regexes are not yet supported in Popcorn
  @compile {:no_warn_undefined, :popcorn_module}
  @compile {:popcorn_patch_private, prompt: 3}
  def prompt(status, prefix, counter) do
    # {mode, prefix} =
    #   if Node.alive?() do
    #     {:popcorn_module.prompt_mode(status, :alive), :popcorn_module.default_prefix(status, prefix)}
    #   else
    #     {:popcorn_module.prompt_mode(status, :default), :popcorn_module.default_prefix(status, prefix)}
    #   end

    prompt = "avm_iex(#{to_string(counter)})>"
    # apply(IEx.Config, mode, [])
    # |> String.replace("%counter", to_string(counter))
    # |> String.replace("%prefix", to_string(prefix))
    # |> String.replace("%node", to_string(node()))

    [prompt, " "]
  end
end
