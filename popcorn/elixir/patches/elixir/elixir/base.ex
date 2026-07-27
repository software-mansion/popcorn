defmodule Base do
  # 1. Patch reason: AtomVM does not support non-byte-aligned bitstring
  # construction. The original base64 decoders build their output from 6-bit
  # segments inside a comprehension (`<<decoded::6, ...>>`), so any decode
  # raises :unsupported at runtime. Encoding is byte-aligned and works.
  # Delegate decoding to OTP's :base64 (verified working on AtomVM), with
  # Elixir-side handling of the url-safe alphabet and the `:padding` /
  # `:ignore` options.

  def decode64(string, opts \\ []) when is_binary(string) do
    {:ok, decode64!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  def decode64!(string, opts \\ []) when is_binary(string) do
    string
    |> filter_ignored(Keyword.get(opts, :ignore))
    |> normalize_padding64(Keyword.get(opts, :padding, true))
    |> base64_decode!()
  end

  def url_decode64(string, opts \\ []) when is_binary(string) do
    {:ok, url_decode64!(string, opts)}
  rescue
    ArgumentError -> :error
  end

  def url_decode64!(string, opts \\ []) when is_binary(string) do
    string
    |> filter_ignored(Keyword.get(opts, :ignore))
    |> url_to_std64()
    |> normalize_padding64(Keyword.get(opts, :padding, true))
    |> base64_decode!()
  end

  defp base64_decode!(string) do
    try do
      :base64.decode(string)
    rescue
      _e ->
        raise ArgumentError,
              "non-alphabet character or incorrect padding in base64-encoded string"
    catch
      _kind, _reason ->
        raise ArgumentError,
              "non-alphabet character or incorrect padding in base64-encoded string"
    end
  end

  defp filter_ignored(string, :whitespace) do
    for <<c <- string>>, c not in ~c" \t\r\n", into: <<>>, do: <<c>>
  end

  defp filter_ignored(string, _ignore), do: string

  defp url_to_std64(string) do
    for <<c <- string>>, into: <<>> do
      case c do
        ?- -> "+"
        ?_ -> "/"
        _ -> <<c>>
      end
    end
  end

  # padding: true — pass through; :base64.decode enforces correct padding.
  defp normalize_padding64(string, true), do: string

  # padding: false — accept unpadded (or padded) input: strip any trailing
  # "=" and re-pad to what :base64.decode expects.
  defp normalize_padding64(string, false) do
    string = String.trim_trailing(string, "=")

    case rem(byte_size(string), 4) do
      0 -> string
      2 -> string <> "=="
      3 -> string <> "="
      _ -> raise ArgumentError, "incorrect padding in base64-encoded string"
    end
  end
end
