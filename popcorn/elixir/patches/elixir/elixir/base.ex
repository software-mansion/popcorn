defmodule Base do
  # Patch reason: Base relies on bitstrings, that AtomVM doesn't support

  @compile {:autoload, false}

  def encode64(data, opts \\ []) when is_binary(data) do
    pad? = Keyword.get(opts, :padding, true)
    :base64.encode(data, %{mode: :standard, padding: pad?})
  end

  def decode64(string, opts \\ []) when is_binary(string) do
    {:ok, decode64!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  def decode64!(string, opts \\ []) when is_binary(string) do
    # :base64 accepts only booleans, while the original accepts any
    # truthy/falsy value here
    pad? = !!Keyword.get(opts, :padding, true)
    decode64base!(string, opts[:ignore], pad?)
  end

  # :base64.decode/2 skips whitespace unconditionally, while the original
  # accepts it only with `ignore: :whitespace`, so it must be rejected upfront
  defp decode64base!(string, nil, pad?) do
    reject_whitespace!(string)
    erl_decode64!(string, nil, pad?)
  end

  defp decode64base!(string, :whitespace, pad?) do
    erl_decode64!(string, :whitespace, pad?)
  end

  defp erl_decode64!(string, ignore, pad?) do
    # with padding: false, :base64 accepts incomplete padding (e.g. a lone
    # trailing "="), while the original requires padding to be either complete
    # or absent, so anything containing "=" is decoded with padding: true
    erl_pad? = pad? or contains_eq?(string)
    :base64.decode(string, %{mode: :standard, padding: erl_pad?})
  rescue
    _e in [ArgumentError, ArithmeticError, CaseClauseError, ErlangError, FunctionClauseError] ->
      # :base64 errors don't say what was wrong with the input, so scan it
      # to raise an ArgumentError matching the original implementation
      string |> remove_ignored(ignore) |> raise_decode64_error!()
  end

  defp remove_ignored(string, nil), do: string

  defp remove_ignored(string, :whitespace) do
    for <<char <- string>>, char not in ~c"\s\t\r\n", into: <<>>, do: <<char>>
  end

  defp contains_eq?(<<?=, _rest::binary>>), do: true
  defp contains_eq?(<<_char, rest::binary>>), do: contains_eq?(rest)
  defp contains_eq?(<<>>), do: false

  defp reject_whitespace!(<<char, _rest::binary>>) when char in ~c"\s\t\r\n" do
    bad_character!(char)
  end

  defp reject_whitespace!(<<_char, rest::binary>>), do: reject_whitespace!(rest)
  defp reject_whitespace!(<<>>), do: :ok

  defp raise_decode64_error!(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char in ~c"+/=" do
    raise_decode64_error!(rest)
  end

  defp raise_decode64_error!(<<char, _rest::binary>>), do: bad_character!(char)
  defp raise_decode64_error!(<<>>), do: raise(ArgumentError, "incorrect padding")

  defp bad_character!(byte) do
    raise ArgumentError,
          "non-alphabet character found: #{inspect(<<byte>>, binaries: :as_strings)} (byte #{byte})"
  end
end
