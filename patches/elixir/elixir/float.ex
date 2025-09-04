defmodule Float do
  # 1. Patch reason: AtomVM fails to load larger literals
  # Original module generates powers up to 104 for both funtions
  @compile {:popcorn_patch_private, power_of_10: 1}
  Enum.reduce(0..20, 1, fn x, acc ->
    def power_of_10(unquote(x)), do: unquote(acc)
    acc * 10
  end)

  def power_of_10(x), do: 10 ** x
  #
  @compile {:popcorn_patch_private, power_of_5: 1}
  Enum.reduce(0..30, 1, fn x, acc ->
    def power_of_5(unquote(x)), do: unquote(acc)
    acc * 5
  end)

  def power_of_5(x), do: 5 ** x
  # End of Patch 1.
end
