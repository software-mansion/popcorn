defmodule Popcorn.BuildLogFormatter do
  @moduledoc """
  Used in Popcorn build and patch process to keep logs short.
  """
  require Logger

  @clear_line "\e[2K\r"
  @overrides [
    format: {__MODULE__, :format},
    colors: [enabled: true],
    metadata: [:app_name]
  ]

  def with_build_logger(f) do
    original_formatter = Logger.default_formatter()
    formatter = Logger.default_formatter(@overrides)
    :logger.update_handler_config(:default, :formatter, formatter)
    f.()
    :logger.update_handler_config(:default, :formatter, original_formatter)
  end

  @spec format(Logger.level(), Logger.message(), Logger.Formatter.date_time_ms(), keyword()) ::
          IO.chardata()
  def format(level, message, timestamp, metadata) do
    {prefix, suffix} = format_control_chars()
    app_name = format_app_name(metadata)

    "#{prefix}[$level#{app_name}] $message#{suffix}"
    |> Logger.Formatter.compile()
    |> Logger.Formatter.format(
      level,
      message,
      timestamp,
      metadata
    )
  end

  defp format_app_name(metadata) do
    case Keyword.fetch(metadata, :app_name) do
      {:ok, app} -> " | #{app}"
      :error -> ""
    end
  end

  defp format_control_chars do
    if interactive_terminal?(), do: {@clear_line, ""}, else: {"", "\n"}
  end

  defp interactive_terminal? do
    with {:ok, true} <- :standard_io |> :io.getopts() |> Keyword.fetch(:terminal),
         true <- IO.ANSI.enabled?(),
         nil <- System.get_env("CI") do
      true
    else
      _ -> false
    end
  end
end
