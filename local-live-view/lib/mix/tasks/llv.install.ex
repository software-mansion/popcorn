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

    * Add the LocalLiveView socket and COOP/COEP headers to your endpoint
    * Import `LocalLiveView.Component` in your web module html_helpers
    * Change app.js script tag to `type="module"`
    * Set up the JS bridge in `assets/js/app.js`
    * Configure esbuild to output ESM format
    * Add the LocalLiveView watcher to your endpoint config in `config/dev.exs`
    * Add `mix llv.build` to your setup alias
    * Generate a `local/` project with a sample HelloLocal local live view
    * Generate a HelloLocalLive LiveView that renders it and route it at `/hello_local`

  `mix setup` will then run `mix llv.build` which copies the LLV JS runtime files
  into your project's `priv/static/assets/js/` and builds the WASM bundle from `local/`.
  """

  @templates_dir Path.expand("../../../priv/templates/llv.install", __DIR__)
  @local_project_dir "local"

  @impl Igniter.Mix.Task
  def igniter(igniter) do
    igniter
    |> inject_endpoint()
    |> inject_web_module()
    |> inject_root_layout()
    |> inject_app_js()
    |> inject_esbuild_format()
    |> inject_esbuild_alias()
    |> inject_tsconfig()
    |> inject_dev_watcher()
    |> inject_setup_alias()
    |> generate_local_project()
    |> generate_hello_live_view()
    |> inject_hello_route()
  end

  # --- Endpoint ---

  defp inject_endpoint(igniter) do
    endpoint = Igniter.Libs.Phoenix.web_module_name(igniter, "Endpoint")

    case Igniter.Project.Module.find_and_update_module(igniter, endpoint, &update_endpoint/1) do
      {:ok, igniter} ->
        igniter

      {:error, igniter} ->
        Igniter.add_warning(
          igniter,
          "Could not find module #{inspect(endpoint)}. Add security headers config manually."
        )
    end
  end

  defp update_endpoint(zipper) do
    if module_source_contains?(zipper, "llv_socket") do
      {:ok, zipper}
    else
      with {:ok, use_zipper} <- Igniter.Code.Function.move_to_function_call(zipper, :use, [1, 2]) do
        zipper = add_wasm_security_headers(use_zipper)
        add_llv_socket(zipper)
      else
        :error ->
          {:warning,
           "Could not find use Phoenix.Endpoint. Add LLV socket and WASM security headers manually."}
      end
    end
  end

  defp add_wasm_security_headers(zipper) do
    Igniter.Code.Common.add_code(
      zipper,
      """
      plug :put_wasm_security_headers

      defp put_wasm_security_headers(conn, _opts) do
        conn
        |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
        |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
      end
      """,
      placement: :after
    )
  end

  defp add_llv_socket(zipper) do
    case find_attr(zipper, :session_options) do
      {:ok, session_opts_zipper} ->
        {:ok,
         Igniter.Code.Common.add_code(
           session_opts_zipper,
           """
           socket "/llv_socket", LocalLiveView.Socket,
             websocket: [connect_info: [session: @session_options]]
           """,
           placement: :after
         )}

      :error ->
        {:warning, "Could not find @session_options. Add LLV socket configuration manually."}
    end
  end

  defp find_attr(zipper, name) do
    case Sourceror.Zipper.find(
           Sourceror.Zipper.topmost(zipper),
           &match?({:@, _, [{^name, _, _}]}, &1)
         ) do
      nil -> :error
      z -> {:ok, z}
    end
  end

  defp module_source_contains?(zipper, str) do
    zipper
    |> Sourceror.Zipper.topmost()
    |> Sourceror.Zipper.node()
    |> Sourceror.to_string()
    |> String.contains?(str)
  end

  # --- Web module ---

  defp inject_web_module(igniter) do
    web_module = Igniter.Libs.Phoenix.web_module(igniter)

    case Igniter.Project.Module.find_and_update_module(igniter, web_module, &update_web_module/1) do
      {:ok, igniter} ->
        igniter

      {:error, igniter} ->
        Igniter.add_warning(
          igniter,
          "Could not find module #{inspect(web_module)}. Add manually to html_helpers: import LocalLiveView.Component"
        )
    end
  end

  defp update_web_module(zipper) do
    if module_source_contains?(zipper, "LocalLiveView.Component") do
      {:ok, zipper}
    else
      anchor =
        find_import_by_suffix(zipper, "CoreComponents") ||
          find_import_by_suffix(zipper, "Phoenix.HTML")

      case anchor do
        {:ok, anchor_zipper} ->
          {:ok, Igniter.Code.Common.add_code(anchor_zipper, "import LocalLiveView.Component")}

        nil ->
          {:warning,
           "Could not find html_helpers imports in web module. Add manually: import LocalLiveView.Component"}
      end
    end
  end

  defp find_import_by_suffix(zipper, suffix) do
    result =
      Igniter.Code.Function.move_to_function_call(zipper, :import, [1, 2], fn call ->
        Igniter.Code.Function.argument_matches_predicate?(call, 0, fn arg ->
          arg
          |> Sourceror.Zipper.node()
          |> Macro.to_string()
          |> String.ends_with?(suffix)
        end)
      end)

    case result do
      {:ok, z} -> {:ok, z}
      :error -> nil
    end
  end

  # --- Root layout ---

  defp inject_root_layout(igniter) do
    web_dir = web_module_dir(igniter)
    path = "lib/#{web_dir}/components/layouts/root.html.heex"

    if Igniter.exists?(igniter, path) do
      replace_in_file(
        igniter,
        path,
        ~s|type="module"|,
        ~s|type="text/javascript"|,
        ~s|type="module"|
      )
    else
      Igniter.add_warning(
        igniter,
        ~s|Could not find #{path}. Change app.js script tag to type="module" manually.|
      )
    end
  end

  defp web_module_dir(igniter) do
    igniter
    |> Igniter.Libs.Phoenix.web_module()
    |> Module.split()
    |> List.last()
    |> Macro.underscore()
  end

  # --- app.js ---

  @llv_js """
  import { LLVEngine } from "local_live_view";
  await LLVEngine.create(liveSocket, { Socket, bundlePaths: ["/assets/js/wasm/bundle.avm"] });
  """

  defp inject_app_js(igniter) do
    path = "assets/js/app.js"

    if Igniter.exists?(igniter, path) do
      Igniter.update_file(igniter, path, fn source ->
        content = Rewrite.Source.get(source, :content)

        if String.contains?(content, "local_live_view") do
          source
        else
          Rewrite.Source.update(
            source,
            :content,
            Regex.replace(
              ~r/liveSocket\.connect\(\);?\n/,
              content,
              "liveSocket.connect();\n\n" <> @llv_js
            )
          )
        end
      end)
    else
      Igniter.add_warning(igniter, """
      Could not find #{path}. Add after liveSocket.connect():
      #{@llv_js}
      """)
    end
  end

  # --- esbuild ESM format ---

  defp inject_esbuild_format(igniter) do
    igniter
    |> inject_esbuild_format_in("config/config.exs")
    |> inject_esbuild_format_in("config/dev.exs")
    |> inject_esbuild_format_in("config/prod.exs")
  end

  defp inject_esbuild_format_in(igniter, path) do
    if Igniter.exists?(igniter, path) do
      replace_in_file(igniter, path, "--format=esm", "--bundle", "--bundle --format=esm")
    else
      igniter
    end
  end

  # --- esbuild alias for path deps ---

  # For path deps, Mix does not materialize them in deps/ — instead, directly
  # use the path from the filesystem. Alias imports to local_live_view with the
  # actual path to the generated JS code.
  # See: https://esbuild.github.io/api/#alias
  @llv_alias_arg ~S|--alias:local_live_view=#{Mix.Project.deps_paths()[:local_live_view]}/priv/static/local_live_view.js|

  defp inject_esbuild_alias(igniter) do
    if llv_path_dep?() do
      igniter
      |> inject_esbuild_alias_in("config/config.exs")
      |> inject_esbuild_alias_in("config/dev.exs")
      |> inject_esbuild_alias_in("config/prod.exs")
    else
      igniter
    end
  end

  defp llv_path_dep? do
    llv_path = Mix.Project.deps_paths()[:local_live_view]
    # Check if local_live_view is a path dependency (not in the default deps/ location)
    llv_path != nil and Path.expand(llv_path) != Path.expand("deps/local_live_view")
  end

  defp inject_esbuild_alias_in(igniter, path) do
    if Igniter.exists?(igniter, path) do
      replace_in_file(
        igniter,
        path,
        "--alias:local_live_view",
        "--alias:@=.",
        "#{@llv_alias_arg} --alias:@=."
      )
    else
      igniter
    end
  end

  # --- tsconfig paths for path deps ---

  # For path deps, tsc cannot resolve type declarations via node_modules since
  # the package lives outside deps/. Add a paths entry pointing to the .d.ts file.
  defp inject_tsconfig(igniter) do
    if llv_path_dep?() do
      inject_tsconfig_in(igniter, "assets/tsconfig.json")
    else
      igniter
    end
  end

  defp inject_tsconfig_in(igniter, path) do
    if Igniter.exists?(igniter, path) do
      llv_dts_path = llv_tsconfig_path()

      Igniter.update_file(igniter, path, fn source ->
        content = Rewrite.Source.get(source, :content)

        if String.contains?(content, "local_live_view") do
          source
        else
          Rewrite.Source.update(
            source,
            :content,
            inject_tsconfig_path_entry(content, llv_dts_path)
          )
        end
      end)
    else
      igniter
    end
  end

  defp inject_tsconfig_path_entry(content, llv_dts_path) do
    entry = ~s|"local_live_view": ["#{llv_dts_path}"]|
    # Strip // line comments so Jason can parse the file. All replacements
    # are done on the original `content` so comments are preserved in output.
    stripped = String.replace(content, ~r|//[^\n]*|, "")

    with {:ok, parsed} <- Jason.decode(stripped),
         {:ok, compiler_opts} <- Map.fetch(parsed, "compilerOptions") do
      if Map.has_key?(compiler_opts, "paths") do
        String.replace(
          content,
          ~r/"paths"\s*:\s*\{/,
          ~s|"paths": {\n      #{entry},|,
          global: false
        )
      else
        String.replace(
          content,
          ~r/"compilerOptions"\s*:\s*\{/,
          ~s|"compilerOptions": {\n    "paths": { #{entry} },|,
          global: false
        )
      end
    else
      # Unparseable JSON or missing "compilerOptions" (extends-only config):
      # return content unchanged; the caller leaves the file as-is.
      _ -> content
    end
  end

  defp llv_tsconfig_path do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    assets_abs = Path.join(File.cwd!(), "assets")
    rel = Path.relative_to(llv_abs, assets_abs, force: true)
    "#{rel}/priv/static/local_live_view.d.ts"
  end

  defp replace_in_file(igniter, path, sentinel, find, replacement) do
    Igniter.update_file(igniter, path, fn source ->
      content = Rewrite.Source.get(source, :content)

      if String.contains?(content, sentinel) do
        source
      else
        Rewrite.Source.update(source, :content, String.replace(content, find, replacement))
      end
    end)
  end

  # --- add watcher to config/dev.exs  ---

  defp inject_dev_watcher(igniter) do
    app_name = Igniter.Project.Application.app_name(igniter)
    endpoint = Igniter.Libs.Phoenix.web_module_name(igniter, "Endpoint")

    Igniter.Project.Config.configure(
      igniter,
      "dev.exs",
      app_name,
      [endpoint, :watchers, :local_live_view],
      {:code, Sourceror.parse_string!("{LocalLiveView.Watcher, :run, []}")},
      updater: &{:ok, &1}
    )
  end

  # --- mix.exs setup alias ---

  defp inject_setup_alias(igniter) do
    Igniter.update_elixir_file(igniter, "mix.exs", fn zipper ->
      if module_source_contains?(zipper, "llv.build") do
        {:ok, zipper}
      else
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
    end)
  end

  defp navigate_to_aliases_list(zipper) do
    with {:ok, mod} <- Igniter.Code.Module.move_to_module_using(zipper, Mix.Project),
         {:ok, list} <- Igniter.Code.Function.move_to_defp(mod, :aliases, 0) do
      {:ok, list}
    end
  end

  # --- local/ project ---

  defp generate_local_project(igniter) do
    if Igniter.exists?(igniter, "#{@local_project_dir}/mix.exs") do
      igniter
    else
      llv_path = llv_path_from_local()

      igniter
      |> copy_template("#{@local_project_dir}/mix.exs", "mix.exs", llv_path: llv_path)
      |> copy_template("#{@local_project_dir}/config/config.exs", "config.exs")
      |> copy_template("#{@local_project_dir}/.formatter.exs", "formatter.exs")
      |> copy_template("#{@local_project_dir}/lib/local/application.ex", "application.ex")
      |> copy_template("#{@local_project_dir}/lib/hello_local.ex", "hello_local.ex")
    end
  end

  defp llv_path_from_local do
    case Mix.Project.deps_paths()[:local_live_view] do
      nil ->
        # Running inside local_live_view itself (e.g. tests) — llv is not a dep.
        ".."

      llv_abs ->
        local_abs = Path.join(File.cwd!(), "local")
        Path.relative_to(llv_abs, local_abs, force: true)
    end
  end

  # --- demo LiveView + route ---

  defp generate_hello_live_view(igniter) do
    live_view = igniter |> Igniter.Libs.Phoenix.web_module() |> Module.concat(HelloLocalLive)

    case Igniter.Project.Module.module_exists(igniter, live_view) do
      {true, igniter} ->
        igniter

      {false, igniter} ->
        web_module = Igniter.Libs.Phoenix.web_module(igniter)
        contents = render_template("hello_local_live.ex", web_module: inspect(web_module))
        Igniter.Project.Module.create_module(igniter, live_view, contents)
    end
  end

  defp inject_hello_route(igniter) do
    {igniter, router} = Igniter.Libs.Phoenix.select_router(igniter)
    route = ~s|live "/hello_local", HelloLocalLive|

    cond do
      is_nil(router) ->
        Igniter.add_warning(
          igniter,
          "Could not find a Phoenix router. Add the demo route manually: #{route}"
        )

      router_contains?(igniter, router, "HelloLocalLive") ->
        igniter

      true ->
        Igniter.Libs.Phoenix.append_to_scope(igniter, "/", route,
          router: router,
          arg2: Igniter.Libs.Phoenix.web_module(igniter),
          with_pipelines: [:browser],
          placement: :after
        )
    end
  end

  defp router_contains?(igniter, router, str) do
    case Igniter.Project.Module.find_module(igniter, router) do
      {:ok, {_igniter, _source, zipper}} -> module_source_contains?(zipper, str)
      {:error, _igniter} -> false
    end
  end

  defp copy_template(igniter, dest, template, bindings \\ []) do
    Igniter.create_new_file(igniter, dest, render_template(template, bindings))
  end

  defp render_template(template, bindings) do
    content = File.read!(Path.join(@templates_dir, template))

    case bindings do
      [] -> content
      bindings -> EEx.eval_string(content, assigns: bindings)
    end
  end
end
