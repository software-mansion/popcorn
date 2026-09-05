# Stub: AtomVM has no :re engine, and Phoenix.Endpoint.Supervisor's
# unconditional url warmup runs a Regex (build_url's host-format warning) —
# a missing NIF aborts the whole VM. This is a verbatim copy of the module
# (phoenix 1.8) with that single host-validation warning removed; every
# other AtomVM-hostile path (static manifest warmup, server adapters,
# browser_open, code reloader) is conditional and never taken in the WASM
# runtime (server: false, no manifest configured).
#
# Goes hand in hand with the Phoenix.Config stub, which the same tree
# starts; see phoenix_config_stub.ex.

module = Phoenix.Endpoint.Supervisor

:code.which(module)
|> case do
  :non_existing -> :ok
  path -> File.rm(path)
end

:code.delete(module)
:code.purge(module)

defmodule module do
  @moduledoc false

  require Logger
  use Supervisor

  def start_link(otp_app, mod, opts \\ []) do
    with {:ok, pid} = ok <- Supervisor.start_link(__MODULE__, {otp_app, mod, opts}, name: mod) do
      # We don't use the defaults in the checks below
      conf = Keyword.merge(Application.get_env(otp_app, mod, []), opts)
      log_access_url(mod, conf)
      browser_open(mod, conf)

      measurements = %{system_time: System.system_time()}
      metadata = %{pid: pid, config: conf, module: mod, otp_app: otp_app}
      :telemetry.execute([:phoenix, :endpoint, :init], measurements, metadata)

      ok
    end
  end

  @doc false
  def init({otp_app, mod, opts}) do
    default_conf = Phoenix.Config.merge(defaults(otp_app, mod), opts)
    env_conf = Phoenix.Config.from_env(otp_app, mod, default_conf)

    secret_conf =
      cond do
        Code.ensure_loaded?(mod) and function_exported?(mod, :init, 2) ->
          {:ok, init_conf} = mod.init(:supervisor, env_conf)
          init_conf

        is_nil(Application.get_env(otp_app, mod)) ->
          env_conf

        true ->
          env_conf
      end

    extra_conf = [
      endpoint_id: :crypto.strong_rand_bytes(16) |> Base.encode64(padding: false),
      pubsub_server: secret_conf[:pubsub_server] || secret_conf[:pubsub][:name]
    ]

    secret_conf = extra_conf ++ secret_conf
    default_conf = extra_conf ++ default_conf

    # Drop all secrets from secret_conf before passing it around
    conf = Keyword.drop(secret_conf, [:secret_key_base])
    server? = server?(conf)

    if server? and conf[:code_reloader] do
      Phoenix.CodeReloader.Server.check_symlinks()
    end

    children =
      config_children(mod, secret_conf, default_conf) ++
        warmup_children(mod) ++
        pubsub_children(mod, conf) ++
        socket_children(mod, conf, :child_spec) ++
        server_children(mod, conf, server?) ++
        socket_children(mod, conf, :drainer_spec) ++
        watcher_children(mod, conf, server?)

    Supervisor.init(children, strategy: :one_for_one)
  end

  defp pubsub_children(mod, conf) do
    pub_conf = conf[:pubsub]

    if pub_conf do
      Logger.warning("The :pubsub key in your #{inspect(mod)} is deprecated.")
    end

    if pub_conf[:adapter] do
      [{Phoenix.PubSub, pub_conf}]
    else
      []
    end
  end

  defp socket_children(endpoint, conf, fun) do
    for {_, socket, opts} <- Enum.uniq_by(endpoint.__sockets__(), &elem(&1, 1)),
        _ = check_origin_or_csrf_checked!(conf, opts),
        spec = apply_or_ignore(socket, fun, [[endpoint: endpoint] ++ opts]),
        spec != :ignore do
      spec
    end
  end

  defp apply_or_ignore(socket, fun, args) do
    # If the module is not loaded, we want to invoke and crash
    if not Code.ensure_loaded?(socket) or function_exported?(socket, fun, length(args)) do
      apply(socket, fun, args)
    else
      :ignore
    end
  end

  defp check_origin_or_csrf_checked!(endpoint_conf, socket_opts) do
    check_origin = endpoint_conf[:check_origin]

    for {transport, transport_opts} <- socket_opts, is_list(transport_opts) do
      check_origin = Keyword.get(transport_opts, :check_origin, check_origin)

      check_csrf = transport_opts[:check_csrf]

      if check_origin == false and check_csrf == false do
        raise ArgumentError,
              "one of :check_origin and :check_csrf must be set to non-false value for " <>
                "transport #{inspect(transport)}"
      end
    end
  end

  defp config_children(mod, conf, default_conf) do
    args = {mod, conf, default_conf, name: Module.concat(mod, "Config")}
    [{Phoenix.Config, args}]
  end

  defp warmup_children(mod) do
    [%{id: :warmup, start: {__MODULE__, :warmup, [mod]}}]
  end

  defp server_children(mod, config, server?) do
    cond do
      server? ->
        adapter = config[:adapter]
        adapter.child_specs(mod, config)

      config[:http] || config[:https] ->
        if System.get_env("RELEASE_NAME") do
          Logger.info(
            "Configuration :server was not enabled for #{inspect(mod)}, http/https services won't start"
          )
        end

        []

      true ->
        []
    end
  end

  defp watcher_children(_mod, conf, server?) do
    watchers = conf[:watchers] || []

    if server? || conf[:force_watchers] do
      Enum.map(watchers, &{Phoenix.Endpoint.Watcher, &1})
    else
      []
    end
  end

  def server?(otp_app, endpoint) when is_atom(otp_app) and is_atom(endpoint) do
    server?(Application.get_env(otp_app, endpoint, []))
  end

  defp server?(conf) when is_list(conf) do
    Keyword.get_lazy(conf, :server, fn ->
      Application.get_env(:phoenix, :serve_endpoints, false)
    end)
  end

  defp defaults(otp_app, module) do
    [
      otp_app: otp_app,

      # Compile-time config
      code_reloader: false,
      debug_errors: false,
      render_errors: [view: render_errors(module), accepts: ~w(html), layout: false],

      # Runtime config
      adapter: Phoenix.Endpoint.Cowboy2Adapter,
      cache_static_manifest: nil,
      check_origin: true,
      http: false,
      https: false,
      reloadable_apps: nil,
      reloadable_compilers: [:phoenix_live_view, :gettext, :elixir, :app],
      secret_key_base: nil,
      static_url: nil,
      url: [host: "localhost", path: "/"],
      cache_manifest_skip_vsn: false,

      # Supervisor config
      watchers: [],
      force_watchers: false
    ]
  end

  defp render_errors(module) do
    module
    |> Module.split()
    |> Enum.at(0)
    |> Module.concat("ErrorView")
  end

  def config_change(endpoint, changed, removed) do
    res = Phoenix.Config.config_change(endpoint, changed, removed)
    warmup(endpoint)
    res
  end

  @invalid_local_url_chars ["\\"]

  def static_lookup(_endpoint, "//" <> _ = path) do
    raise_invalid_path(path)
  end

  def static_lookup(_endpoint, "/" <> _ = path) do
    if String.contains?(path, @invalid_local_url_chars) do
      raise ArgumentError, "unsafe characters detected for path #{inspect(path)}"
    else
      {:nocache, {path, nil}}
    end
  end

  def static_lookup(_endpoint, path) when is_binary(path) do
    raise_invalid_path(path)
  end

  defp raise_invalid_path(path) do
    raise ArgumentError, "expected a path starting with a single / but got #{inspect(path)}"
  end

  defp host_to_binary({:system, env_var}), do: host_to_binary(System.get_env(env_var))
  defp host_to_binary(host), do: host

  defp port_to_integer({:system, env_var}), do: port_to_integer(System.get_env(env_var))
  defp port_to_integer(port) when is_binary(port), do: String.to_integer(port)
  defp port_to_integer(port) when is_integer(port), do: port

  def warmup(endpoint) do
    warmup_persistent(endpoint)

    try do
      if manifest = cache_static_manifest(endpoint) do
        warmup_static(endpoint, manifest)
      end
    rescue
      e -> Logger.error("Could not warm up static assets: #{Exception.message(e)}")
    end

    # To prevent a race condition where the socket listener is already started
    # but the config not warmed up, we run warmup/1 as a child in the supervision
    # tree. As we don't actually want to start a process, we return :ignore here.
    :ignore
  end

  defp warmup_persistent(endpoint) do
    url_config = endpoint.config(:url)
    static_url_config = endpoint.config(:static_url) || url_config

    struct_url = build_url(endpoint, url_config)
    host = host_to_binary(url_config[:host] || "localhost")
    path = empty_string_if_root(url_config[:path] || "/")
    script_name = String.split(path, "/", trim: true)

    static_url = build_url(endpoint, static_url_config) |> String.Chars.URI.to_string()
    static_path = empty_string_if_root(static_url_config[:path] || "/")

    :persistent_term.put({Phoenix.Endpoint, endpoint}, %{
      struct_url: struct_url,
      url: String.Chars.URI.to_string(struct_url),
      host: host,
      path: path,
      script_name: script_name,
      static_path: static_path,
      static_url: static_url
    })
  end

  defp empty_string_if_root("/"), do: ""
  defp empty_string_if_root(other), do: other

  defp build_url(endpoint, url) do
    https = endpoint.config(:https)
    http = endpoint.config(:http)

    {scheme, port} =
      cond do
        https -> {"https", https[:port] || 443}
        http -> {"http", http[:port] || 80}
        true -> {"http", 80}
      end

    scheme = url[:scheme] || scheme
    host = host_to_binary(url[:host] || "localhost")
    port = port_to_integer(url[:port] || port)

    # The original warns here when the host looks like it embeds a port,
    # via a Regex — the one AtomVM-incompatible expression this stub exists
    # to remove (no :re engine; a missing NIF aborts the VM).

    %URI{scheme: scheme, port: port, host: host}
  end

  defp warmup_static(endpoint, %{"latest" => latest, "digests" => digests}) do
    Phoenix.Config.put(endpoint, :cache_static_manifest_latest, latest)
    with_vsn? = !endpoint.config(:cache_manifest_skip_vsn)

    Enum.each(latest, fn {key, _} ->
      Phoenix.Config.cache(endpoint, {:__phoenix_static__, "/" <> key}, fn _ ->
        {:cache, static_cache(digests, Map.get(latest, key), with_vsn?)}
      end)
    end)
  end

  defp warmup_static(_endpoint, _manifest) do
    raise ArgumentError, "expected cache manifest to include 'latest' and 'digests' keys"
  end

  defp static_cache(digests, value, true) do
    {"/#{value}?vsn=d", static_integrity(digests[value]["sha512"])}
  end

  defp static_cache(digests, value, false) do
    {"/#{value}", static_integrity(digests[value]["sha512"])}
  end

  defp static_integrity(nil), do: nil
  defp static_integrity(sha), do: "sha512-#{sha}"

  defp cache_static_manifest(endpoint) do
    if inner = endpoint.config(:cache_static_manifest) do
      {app, inner} =
        case inner do
          {_, _} = inner -> inner
          inner when is_binary(inner) -> {endpoint.config(:otp_app), inner}
          _ -> raise ArgumentError, ":cache_static_manifest must be a binary or a tuple"
        end

      outer = Application.app_dir(app, inner)

      if File.exists?(outer) do
        outer |> File.read!() |> Phoenix.json_library().decode!()
      else
        raise ArgumentError,
              "could not find static manifest at #{inspect(outer)}. " <>
                "Run \"mix phx.digest\" after building your static files " <>
                "or remove the \"cache_static_manifest\" configuration from your config files."
      end
    else
      nil
    end
  end

  defp log_access_url(endpoint, conf) do
    if Keyword.get(conf, :log_access_url, true) && server?(conf) do
      Logger.info("Access #{inspect(endpoint)} at #{endpoint.url()}")
    end
  end

  defp browser_open(endpoint, conf) do
    if Application.get_env(:phoenix, :browser_open, false) && server?(conf) do
      url = endpoint.url()

      {cmd, args} =
        case :os.type() do
          {:win32, _} -> {"cmd", ["/c", "start", url]}
          {:unix, :darwin} -> {"open", [url]}
          {:unix, _} -> {"xdg-open", [url]}
        end

      System.cmd(cmd, args)
    end
  end
end
