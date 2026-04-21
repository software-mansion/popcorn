defmodule Mix.Tasks.Llv.Install do
  use Igniter.Mix.Task

  @shortdoc "Installs LocalLiveView into a Phoenix project"

  @moduledoc """
  Installs LocalLiveView into an existing Phoenix project.

  ## Usage

      mix phx.new my_app
      cd my_app
      # add {:local_live_view, ...} to mix.exs deps, then:
      mix llv.install
      mix setup
      mix phx.server

  This task will:

    * Add the LocalLiveView socket to your endpoint
    * Add COOP/COEP security headers required for WASM
    * Register the channel registry in your application supervisor
    * Import `LocalLiveView.Component` in your web module html_helpers
    * Change app.js script tag to `type="module"`
    * Set up the JS bridge in `assets/js/app.js`
    * Configure the JS build pipeline (`assets/build.mjs`, `assets/package.json`)
    * Vendor `local_live_view.js` into `assets/vendor/`
    * Generate a `local/` project with a sample HelloLocal component
  """

  @popcorn_js_version "0.3.0-rc1"

  @impl Igniter.Mix.Task
  def igniter(igniter) do
    llv_path = llv_path_from_local()

    igniter
    |> inject_endpoint()
    |> inject_application()
    |> inject_web_module()
    |> inject_root_layout()
    |> inject_app_js()
    |> inject_package_json()
    |> inject_mix_esbuild()
    |> inject_dev_watcher()
    |> generate_build_mjs()
    |> generate_vendor()
    |> generate_local_project(llv_path)
  end

  # --- Endpoint ---

  defp inject_endpoint(igniter) do
    endpoint = Module.concat(Igniter.Libs.Phoenix.web_module(igniter), "Endpoint")

    case Igniter.Project.Module.find_and_update_module(igniter, endpoint, fn zipper ->
           with {:ok, zipper} <- add_llv_socket(zipper),
                {:ok, zipper} <- add_wasm_headers(zipper) do
             {:ok, zipper}
           end
         end) do
      {:ok, igniter} ->
        igniter

      {:error, igniter} ->
        Igniter.add_warning(igniter, """
        Could not find endpoint module #{inspect(endpoint)}. Add manually:

            socket "/llv_socket", LocalLiveView.Socket, websocket: true

            plug :put_wasm_security_headers

            defp put_wasm_security_headers(conn, _opts) do
              conn
              |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
              |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
            end
        """)
    end
  end

  defp add_llv_socket(zipper) do
    if module_source_contains?(zipper, "LocalLiveView.Socket") do
      {:ok, zipper}
    else
      case Igniter.Code.Function.move_to_function_call_in_current_scope(zipper, :plug, [1, 2]) do
        {:ok, plug_zipper} ->
          {:ok,
           Igniter.Code.Common.add_code(
             plug_zipper,
             ~s[socket "/llv_socket", LocalLiveView.Socket, websocket: true],
             placement: :before
           )}

        :error ->
          {:warning, "No plug call found in endpoint to insert socket before"}
      end
    end
  end

  defp add_wasm_headers(zipper) do
    if module_source_contains?(zipper, "put_wasm_security_headers") do
      {:ok, zipper}
    else
      case Igniter.Code.Module.move_to_use(zipper, Phoenix.Endpoint) do
        {:ok, use_zipper} ->
          zipper_with_plug =
            Igniter.Code.Common.add_code(use_zipper, "plug :put_wasm_security_headers")

          last =
            zipper_with_plug
            |> Sourceror.Zipper.down()
            |> Sourceror.Zipper.rightmost()

          {:ok,
           Igniter.Code.Common.add_code(last, """
           defp put_wasm_security_headers(conn, _opts) do
             conn
             |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
             |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
           end
           """)}

        :error ->
          {:warning, "Could not find 'use Phoenix.Endpoint' to insert WASM headers"}
      end
    end
  end

  defp module_source_contains?(zipper, str) do
    zipper
    |> Sourceror.Zipper.topmost()
    |> Sourceror.Zipper.node()
    |> Sourceror.to_string()
    |> String.contains?(str)
  end

  # --- Application ---

  defp inject_application(igniter) do
    Igniter.Project.Application.add_new_child(
      igniter,
      {Registry, keys: :unique, name: LocalLiveView.ChannelRegistry}
    )
  end

  # --- Web module ---

  defp inject_web_module(igniter) do
    case Path.wildcard("lib/*_web.ex") do
      [path | _] ->
        Igniter.update_elixir_file(igniter, path, fn zipper ->
          if module_source_contains?(zipper, "LocalLiveView.Component") do
            {:ok, zipper}
          else
            anchor =
              find_import_by_suffix(zipper, "CoreComponents") ||
                find_import_by_suffix(zipper, "Phoenix.HTML")

            case anchor do
              {:ok, anchor_zipper} ->
                {:ok,
                 Igniter.Code.Common.add_code(
                   anchor_zipper,
                   "import LocalLiveView.Component"
                 )}

              nil ->
                {:warning,
                 "Could not find html_helpers imports in #{path}. Add manually: import LocalLiveView.Component"}
            end
          end
        end)

      [] ->
        Igniter.add_warning(
          igniter,
          "Could not find *_web.ex. Add manually to html_helpers: import LocalLiveView.Component"
        )
    end
  end

  defp find_import_by_suffix(zipper, suffix) do
    case Igniter.Code.Function.move_to_function_call(zipper, :import, [1, 2], fn call ->
           Igniter.Code.Function.argument_matches_predicate?(call, 0, fn arg ->
             arg
             |> Sourceror.Zipper.node()
             |> Macro.to_string()
             |> String.ends_with?(suffix)
           end)
         end) do
      {:ok, z} -> {:ok, z}
      :error -> nil
    end
  end

  # --- Root layout ---

  defp inject_root_layout(igniter) do
    case Path.wildcard("lib/*_web/components/layouts/root.html.heex") do
      [path | _] ->
        Igniter.update_file(igniter, path, fn source ->
          content = Rewrite.Source.get(source, :content)

          if String.contains?(content, ~s|type="module"|) do
            source
          else
            Rewrite.Source.update(
              source,
              :content,
              String.replace(content, ~s|type="text/javascript"|, ~s|type="module"|)
            )
          end
        end)

      [] ->
        Igniter.add_warning(
          igniter,
          ~s|Could not find root.html.heex. Change app.js script tag to type="module" manually.|
        )
    end
  end

  # --- app.js ---
  defp inject_app_js(igniter) do
    case Path.wildcard("assets/js/app.js") do
      [path | _] ->
        Igniter.update_file(igniter, path, fn source ->
          content = Rewrite.Source.get(source, :content)

          if String.contains?(content, "local_live_view") do
            source
          else
            llv_js =
              ~s[\nimport { setup } from "vendor/local_live_view";\n] <>
                ~s[setup(liveSocket, { Socket, bundlePath: "/assets/js/wasm/bundle.avm" });\n]

            Rewrite.Source.update(
              source,
              :content,
              Regex.replace(
                ~r/liveSocket\.connect\(\);?\n/,
                content,
                "liveSocket.connect();\n" <> llv_js
              )
            )
          end
        end)

      [] ->
        Igniter.add_warning(igniter, """
        Could not find assets/js/app.js. Add after liveSocket.connect():
            import { setup } from "vendor/local_live_view";
            setup(liveSocket, { Socket, bundlePath: "/assets/js/wasm/bundle.avm" });
        """)
    end
  end

  # --- package.json ---

  defp inject_package_json(igniter) do
    path = "assets/package.json"

    if File.exists?(path) do
      Igniter.update_file(igniter, path, fn source ->
        content = Rewrite.Source.get(source, :content)

        if String.contains?(content, "@swmansion/popcorn") do
          source
        else
          new_content =
            content
            |> inject_after_pattern(
              ~r/"devDependencies": \{\n/,
              ~s|    "@swmansion/popcorn": "#{@popcorn_js_version}",\n|
            )
            |> inject_after_pattern(
              ~r/"devDependencies": \{\n/,
              ~s|    "esbuild": "^0.25.0",\n|
            )

          Rewrite.Source.update(source, :content, new_content)
        end
      end)
    else
      copy_template(igniter, "assets/package.json", "package.json",
        popcorn_version: @popcorn_js_version
      )
    end
  end

  # --- mix.exs ---

  defp inject_mix_esbuild(igniter) do
    Igniter.update_elixir_file(igniter, "mix.exs", fn zipper ->
      if module_source_contains?(zipper, "llv.build") do
        {:ok, zipper}
      else
        with {:ok, zipper} <- insert_llv_build_in_setup(zipper),
             {:ok, zipper} <- insert_npm_in_assets_setup(Sourceror.Zipper.topmost(zipper)),
             {:ok, zipper} <- prepend_build_js_in_assets_build(Sourceror.Zipper.topmost(zipper)),
             {:ok, zipper} <- prepend_build_js_in_assets_deploy(Sourceror.Zipper.topmost(zipper)),
             {:ok, zipper} <- ensure_npm_install_helper(Sourceror.Zipper.topmost(zipper)),
             {:ok, zipper} <- ensure_build_js_helper(Sourceror.Zipper.topmost(zipper)) do
          {:ok, zipper}
        end
      end
    end)
  end

  defp navigate_to_aliases_list(zipper) do
    with {:ok, mod} <- Igniter.Code.Module.move_to_module_using(zipper, Mix.Project),
         {:ok, list} <- Igniter.Code.Function.move_to_defp(mod, :aliases, 0) do
      {:ok, list}
    end
  end

  defp insert_llv_build_in_setup(zipper) do
    with {:ok, aliases} <- navigate_to_aliases_list(zipper),
         {:ok, setup} <- Igniter.Code.Keyword.get_key(aliases, :setup) do
      case Igniter.Code.List.move_to_list_item(setup, fn z ->
             z |> Sourceror.Zipper.node() |> Macro.to_string() == ~s["deps.get"]
           end) do
        {:ok, deps_z} ->
          {:ok, Sourceror.Zipper.insert_right(deps_z, "llv.build")}

        :error ->
          Igniter.Code.List.prepend_to_list(setup, "llv.build")
      end
    end
  end

  defp insert_npm_in_assets_setup(zipper) do
    with {:ok, aliases} <- navigate_to_aliases_list(zipper),
         {:ok, list} <- Igniter.Code.Keyword.get_key(aliases, :"assets.setup") do
      Igniter.Code.List.append_new_to_list(list, quote(do: &npm_install/1))
    end
  end

  defp prepend_build_js_in_assets_build(zipper) do
    with {:ok, aliases} <- navigate_to_aliases_list(zipper),
         {:ok, list} <- Igniter.Code.Keyword.get_key(aliases, :"assets.build") do
      Igniter.Code.List.prepend_new_to_list(list, quote(do: &build_js/1))
    end
  end

  defp prepend_build_js_in_assets_deploy(zipper) do
    with {:ok, aliases} <- navigate_to_aliases_list(zipper),
         {:ok, list} <- Igniter.Code.Keyword.get_key(aliases, :"assets.deploy") do
      Igniter.Code.List.prepend_new_to_list(list, quote(do: &build_js/1))
    end
  end

  defp ensure_npm_install_helper(zipper) do
    if module_source_contains?(zipper, "defp npm_install") do
      {:ok, zipper}
    else
      with {:ok, mod} <- Igniter.Code.Module.move_to_module_using(zipper, Mix.Project) do
        last = Igniter.Code.Common.rightmost(mod)

        {:ok,
         Igniter.Code.Common.add_code(last, """
         defp npm_install(_) do
           {_, 0} =
             System.cmd("npm", ["install"],
               cd: Path.join(File.cwd!(), "assets"),
               into: IO.stream(:stdio, :line),
               stderr_to_stdout: true
             )
         end
         """)}
      end
    end
  end

  defp ensure_build_js_helper(zipper) do
    if module_source_contains?(zipper, "defp build_js") do
      {:ok, zipper}
    else
      with {:ok, mod} <-
             Igniter.Code.Module.move_to_module_using(
               Sourceror.Zipper.topmost(zipper),
               Mix.Project
             ) do
        last = Igniter.Code.Common.rightmost(mod)

        {:ok,
         Igniter.Code.Common.add_code(last, """
         defp build_js(_) do
           {_, 0} =
             System.cmd("node", ["build.mjs"],
               cd: Path.join(File.cwd!(), "assets"),
               env: [{"MIX_BUILD_PATH", Mix.Project.build_path()}],
               into: IO.stream(:stdio, :line),
               stderr_to_stdout: true
             )
         end
         """)}
      end
    end
  end

  # --- config/dev.exs ---

  defp inject_dev_watcher(igniter) do
    Igniter.update_elixir_file(igniter, "config/dev.exs", fn zipper ->
      content =
        zipper
        |> Sourceror.Zipper.topmost()
        |> Sourceror.Zipper.node()
        |> Sourceror.to_string()

      new_content =
        content
        |> replace_esbuild_watcher()
        |> disable_web_console_logger()

      if new_content == content do
        {:ok, zipper}
      else
        {:ok,
         Igniter.Code.Common.replace_code(
           Sourceror.Zipper.topmost(zipper),
           Sourceror.parse_string!(new_content)
         )}
      end
    end)
  end

  defp replace_esbuild_watcher(content) do
    if String.contains?(content, "build.mjs") do
      content
    else
      node_watcher =
        "node: [\n" <>
          ~s|      "build.mjs",\n| <>
          ~s|      "--watch",\n| <>
          ~s|      cd: Path.expand("../assets", __DIR__),\n| <>
          ~s|      env: %{"MIX_BUILD_PATH" => Mix.Project.build_path()}\n| <>
          "    ],\n"

      String.replace(content, ~r/esbuild: \{Esbuild,[^\}]+\},?\n/, node_watcher)
    end
  end

  defp disable_web_console_logger(content) do
    String.replace(content, "web_console_logger: true", "web_console_logger: false")
  end

  # --- assets/build.mjs ---

  defp generate_build_mjs(igniter) do
    Igniter.create_new_file(igniter, "assets/build.mjs", read_template("build.mjs"))
  end

  # --- assets/vendor/local_live_view.js ---

  defp generate_vendor(igniter) do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    src = Path.join(llv_abs, "assets/local_live_view.js")

    Igniter.create_new_file(igniter, "assets/vendor/local_live_view.js", File.read!(src))
  end

  # --- local/ project ---

  defp generate_local_project(igniter, llv_path) do
    if File.exists?("local") do
      igniter
    else
      File.mkdir_p!("local/config")
      File.mkdir_p!("local/lib/local")

      igniter
      |> copy_template("local/mix.exs", "mix.exs", llv_path: llv_path)
      |> copy_template("local/config/config.exs", "config.exs")
      |> copy_template("local/.formatter.exs", "formatter.exs")
      |> copy_template("local/lib/local/application.ex", "application.ex")
      |> copy_template("local/lib/hello_local.ex", "hello_local.ex")
    end
  end

  defp llv_path_from_local do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    local_abs = Path.join(File.cwd!(), "local")
    Path.relative_to(llv_abs, local_abs, force: true)
  end

  # --- Helpers ---

  defp inject_after_pattern(content, pattern, injection) do
    case Regex.run(pattern, content, return: :index) do
      [{start, len}] ->
        {before, rest} = String.split_at(content, start + len)
        before <> injection <> rest

      _ ->
        content
    end
  end

  @templates_dir Path.expand("../../../priv/templates/llv.install", __DIR__)

  defp copy_template(igniter, dest, template, bindings \\ []) do
    content = File.read!(Path.join(@templates_dir, template))

    content =
      if bindings == [] do
        content
      else
        EEx.eval_string(content, assigns: bindings)
      end

    Igniter.create_new_file(igniter, dest, content)
  end

  defp read_template(template) do
    File.read!(Path.join(@templates_dir, template))
  end
end
