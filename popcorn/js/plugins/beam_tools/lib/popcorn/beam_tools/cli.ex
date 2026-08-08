defmodule Popcorn.BeamTools.CLI do
  alias Popcorn.BeamTools.Packager

  @options [
    root_dir: :string,
    entrypoint_app: :string,
    out_dir: :string,
    manifest_path: :string,
    strip: :boolean
  ]

  @required_options [:root_dir, :out_dir, :manifest_path]

  def main(argv) do
    if String.to_integer(System.otp_release()) < 27 do
      IO.puts(:stderr, "tarballs.exs requires host OTP >= 27 for the built-in :json module")
      System.halt(1)
    end

    argv
    |> run()
    |> report()
    |> encode_json()
    |> IO.puts()
  end

  def run(argv) do
    with {:ok, options} <- parse_argv(argv) do
      Packager.run(options)
    end
  end

  defp parse_argv(argv) do
    {opts, tar_paths, invalid} = OptionParser.parse(argv, strict: @options)
    missing_opts = Enum.reject(@required_options, &Keyword.has_key?(opts, &1))

    case {invalid, missing_opts} do
      {[], []} ->
        options =
          opts
          |> Map.new()
          |> Map.put_new(:entrypoint_app, nil)
          |> Map.put_new(:strip, false)
          |> Map.put(:tar_paths, tar_paths)

        {:ok, options}

      _ ->
        bad_args(invalid, missing_opts)
    end
  end

  defp bad_args(invalid, missing_opts) do
    invalid =
      Enum.map(invalid, fn {option, value} ->
        %{option: option, value: value}
      end)

    missing = Enum.map(missing_opts, &to_string/1)

    {:error, %{code: "bad_args", invalid: invalid, missing: missing}}
  end

  defp report({:ok, report}), do: report
  defp report({:error, error}), do: %{ok: false, error: error}

  defp encode_json(term) do
    term |> :json.encode() |> IO.iodata_to_binary()
  end
end
