defmodule Logger.App do
  # Patch reason: using custom atomvm logger

  @compile {:no_warn_undefined, :atomvm_logger_manager}

  def start(_type, _args) do
    # Patch reason: logger fails to convert time to the local time
    # due to missing erlang:universaltime_to_localtime NIF
    # and possibly other time-related NIFs
    Application.put_env(:logger, :utc_log, true)

    level = Application.get_env(:logger, :level, :debug)
    :atomvm_logger_manager.start_link(%{log_level: level})
  end

  def stop(_state) do
    :ok
  end
end
