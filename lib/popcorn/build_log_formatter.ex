defmodule Popcorn.BuildLogFormatter do
  @moduledoc """
  Used in Popcorn build and patch process to keep logs short.
  """
  require Logger

  @clear_line "#{IO.ANSI.clear_line()}\r"
  @overrides [
    format: {__MODULE__, :format},
    colors: [enabled: true],
    metadata: [:app_name]
  ]

  def enable() do
    original_formatter = Logger.default_formatter()
    formatter = Logger.default_formatter(@overrides)
    :logger.update_handler_config(:default, :formatter, formatter)
    original_formatter
  end

  def disable(config) do
    :logger.update_handler_config(:default, :formatter, config)
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
    is_not_ci = "CI" |> System.get_env() |> is_nil()

    IO.ANSI.enabled?() and is_not_ci
  end
end
