import Kernel, except: [inspect: 2]

defmodule Logger.Formatter do
  @compile {:no_warn_undefined, :popcorn_module}

  defstruct [:template, :truncate, :metadata, :colors, :utc_log?]

  @default_pattern "\n$time $metadata[$level] $message\n"

  # Patch reason: the original implementation uses regex, which is not supported in AtomVM
  def compile(nil), do: compile(@default_pattern)
  def compile({mod, fun}) when is_atom(mod) and is_atom(fun), do: {mod, fun}

  def compile(str) when is_binary(str) do
    for part <- parse_pattern(str, []) do
      case part do
        "$" <> code -> :popcorn_module.compile_code(String.to_atom(code))
        _ -> part
      end
    end
  end

  defp parse_pattern(<<char>> <> rest, []) do
    parse_pattern(rest, [<<char>>])
  end

  defp parse_pattern("$" <> rest, acc) do
    parse_pattern(rest, ["$" | acc])
  end

  defp parse_pattern(<<char>> <> rest, ["$" <> _key | _rest] = acc) when char not in ?a..?z do
    parse_pattern(rest, [<<char>> | acc])
  end

  defp parse_pattern(<<char>> <> rest, [value | acc]) do
    parse_pattern(rest, [value <> <<char>> | acc])
  end

  defp parse_pattern(<<>>, acc) do
    Enum.reverse(acc)
  end
end
