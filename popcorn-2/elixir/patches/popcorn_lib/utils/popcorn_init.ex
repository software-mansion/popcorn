defmodule Popcorn.Init do
  # The module below tries to mimic BEAMs boot script, then start user's app
  @compile autoload: false, no_warn_undefined: [Popcorn.Wasm, :popcorn_app_env]

  # --- Static boot (experimental, config :popcorn, static_boot: true) --------
  #
  # The generated boot module (see Popcorn.create_static_boot_module/4) calls
  # static_pre_boot/1, then starts every application with a literal
  # `Mod.start(:normal, args)` call in dependency order, then calls
  # static_finalize/2. No application_controller, application_master or
  # application:load/ensure_all_started is involved: app env is served from an
  # ac_tab-shaped ETS table and IO goes to a minimal group-leader IO server
  # (both in :popcorn_app_env).

  def static_boot(env_entries, app, start_module, start_apps_fun) do
    :popcorn_app_env.debug("boot module running")
    :ok = :popcorn_app_env.init(env_entries)

    # Application processes inherit the group leader from this process. Without
    # application_masters something must serve io_requests (io:format & co).
    io_server = :popcorn_app_env.start_io_server()
    :erlang.group_leader(io_server, self())
    :popcorn_app_env.debug("group leader installed")

    # Runs the generated module's literal Mod.start(:normal, args) calls.
    :ok = start_apps_fun.()
    :popcorn_app_env.debug("all applications started")

    Popcorn.Wasm.send_event("popcorn_elixir_ready")

    case start_module do
      nil -> static_default_start(app)
      module -> module.start()
    end
  rescue
    e ->
      :popcorn_app_env.debug("boot failed: #{Exception.format(:error, e, __STACKTRACE__)}")
      :erlang.display({e, __STACKTRACE__})
      reraise e, __STACKTRACE__
  end

  # Mirrors default_start/1: block while the root app's supervisor is alive,
  # so the boot process (which all app supervisors are linked to) stays up.
  defp static_default_start(app) do
    case :popcorn_app_env.get_supervisor(app) do
      :undefined ->
        :ok

      {:ok, pid} ->
        ref = Process.monitor(pid)

        receive do
          {:DOWN, ^ref, :process, _object, reason} -> reason
        end
    end
  end

  # --- Classic boot -----------------------------------------------------------

  def init(config) do
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
