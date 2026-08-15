defmodule :popcorn_app_env do
  # Data plane for the static boot (see Popcorn.Init.init_static/1): replaces
  # the application_controller with a plain ETS table and the application_master
  # group leaders with a minimal IO server.
  #
  # The table is named `ac_tab` and uses the same `{{:env, App, Key}, Value}`
  # row shape as the real controller, so the stock `application:get_env/2,3`
  # and `application:get_all_env/1` read paths (pure ets lookups in
  # application_controller.erl) keep working without the controller process.
  # `{:loaded, App}` rows are NOT inserted: `Application.spec/1` returns nil
  # for every app, and `application:ensure_all_started/1` is not supported at
  # runtime — under the static boot everything is already running.

  @compile {:no_warn_undefined, :console}

  @doc "Creates ac_tab and registers the given `{app, key, value}` env entries."
  def init(env_entries) do
    debug("creating ac_tab")
    :ets.new(:ac_tab, [:set, :public, :named_table, {:read_concurrency, true}])
    :ets.insert(:ac_tab, {:popcorn_static_boot, true})

    for {app, key, value} <- env_entries do
      :ets.insert(:ac_tab, {{:env, app, key}, value})
    end

    debug("registered #{length(env_entries)} env entries")
    :ok
  end

  def debug(msg) do
    :console.print("[static_boot] #{msg}\n")
  catch
    _, _ -> :ok
  end

  @doc """
  Records the result of a `Mod.start(:normal, args)` call made by the generated
  static boot module. Raises when the application failed to start.
  """
  def app_started(app, {:ok, pid}) when is_pid(pid), do: register(app, pid)
  def app_started(app, {:ok, pid, _state}) when is_pid(pid), do: register(app, pid)

  def app_started(app, other) do
    debug("could not start #{app}: #{inspect(other)}")
    :erlang.error({:could_not_start_application, app, other})
  end

  defp register(app, pid) do
    :ets.insert(:ac_tab, {{:popcorn_app_sup, app}, pid})
    debug("started #{app}")
    :ok
  end

  @doc "The root supervisor an app registered at boot, or :undefined."
  def get_supervisor(app) do
    case :ets.lookup(:ac_tab, {:popcorn_app_sup, app}) do
      [{_, pid}] -> {:ok, pid}
      [] -> :undefined
    end
  end

  def started_applications do
    for {{:popcorn_app_sup, app}, _pid} <- :ets.tab2list(:ac_tab), do: app
  end

  @doc """
  Spawns a minimal IO server and returns its pid. Under the static boot there
  are no application_masters, whose patched relay used to serve io_requests
  (see application_master.erl patch); this process takes over that role as the
  group leader all application processes inherit.
  """
  def start_io_server do
    spawn(&io_loop/0)
  end

  defp io_loop do
    receive do
      {:io_request, from, reply_as, request} ->
        send(from, {:io_reply, reply_as, io_request(request)})

      _other ->
        :ok
    end

    io_loop()
  end

  defp io_request({:put_chars, _encoding, chars}), do: print(chars)

  defp io_request({:put_chars, chars}), do: print(chars)

  defp io_request({:put_chars, _encoding, mod, fun, args}) do
    print(apply(mod, fun, args))
  catch
    _, _ -> {:error, {:put_chars, mod, fun, args}}
  end

  defp io_request({:requests, requests}) do
    Enum.reduce(requests, :ok, fn
      request, :ok -> io_request(request)
      _request, error -> error
    end)
  end

  defp io_request({:setopts, _opts}), do: :ok
  defp io_request(:getopts), do: []
  defp io_request({:get_geometry, _}), do: {:error, :enotsup}

  # Input requests (get_chars/get_line/get_until/...) — no stdin in the browser.
  defp io_request(_request), do: {:error, :enotsup}

  defp print(chars) do
    :console.print(chars)
    :ok
  catch
    _, _ -> {:error, :put_chars}
  end
end
