defmodule Popcorn.Boot do
  # The module below tries to mimic BEAMs boot script, then start user's app
  @compile autoload: false, no_warn_undefined: [Popcorn.AppConfig, Popcorn.Wasm]

  def start() do
    config = Popcorn.AppConfig.get_config()
    start_apps(config.app, config.apps_specs)
    Popcorn.Wasm.send_event("popcorn_elixir_ready")

    case config.start_module do
      nil -> default_start(config.app)
      module -> module.start()
    end
  rescue
    e ->
      :erlang.display({e, __STACKTRACE__})
      reraise e, __STACKTRACE__
  end

  defp default_start(app) do
    case :application.get_supervisor(app) do
      :undefined ->
        :ok

      {:ok, pid} ->
        ref = Process.monitor(pid)

        receive do
          {:DOWN, ^ref, :process, _object, reason} -> reason
        end
    end
  end

  defp start_apps(app, specs) do
    # TODO: Default boot script starts `:heart` process, but unless -heart flag is passed, it will return `:ignore`
    # :ignore = :heart.start()
    # TODO: Default boot script starts :logger_server, uncomment line below when :logger app is supported
    # {:ok, _pid} = :logger_server.start_link()

    {:ok, _ac} = :application_controller.start({:application, :kernel, specs[:kernel]})

    for {app, spec} <- specs, app != :kernel do
      :ok = :application.load({:application, app, spec})
    end

    :ok = :application.start_boot(:kernel, :permanent)
    :ok = :application.start_boot(:stdlib, :permanent)

    {:ok, _apps} = :application.ensure_all_started(app, :permanent)

    :ok
  end
end
