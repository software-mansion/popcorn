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
    * Generate a `local/` project with a sample HelloLocal component

  `mix setup` will then run `mix llv.build` which bundles the JS assets
  and builds the WASM bundle from `local/`.
  """

  @templates_dir Path.expand("../../../priv/templates/llv.install", __DIR__)

  @impl Igniter.Mix.Task
  def igniter(igniter) do
    llv_path = llv_path_from_local()

    igniter
    |> inject_endpoint()
    |> inject_web_module()
    |> inject_root_layout()
    |> inject_app_js()
    |> inject_esbuild_format()
    |> inject_dev_watcher()
    |> inject_setup_alias()
    |> generate_local_project(llv_path)
  end

  defp llv_path_from_local do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    local_abs = Path.join(File.cwd!(), "local")
    Path.relative_to(llv_abs, local_abs, force: true)
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
      case Igniter.Code.Function.move_to_function_call(zipper, :use, [1, 2]) do
        {:ok, use_zipper} ->
          {:ok,
           Igniter.Code.Common.add_code(
             use_zipper,
             """

             plug :put_wasm_security_headers

             socket "/llv_socket", LocalLiveView.Socket, websocket: true

             defp put_wasm_security_headers(conn, _opts) do
               conn
               |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
               |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
             end
             """,
             placement: :after
           )}

        :error ->
          {:warning,
           "Could not find use Phoenix.Endpoint in endpoint. Add LLV socket and WASM security headers manually."}
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
    case Path.wildcard("lib/*_web/components/layouts/root.html.heex") do
      [path | _] ->
        replace_in_file(
          igniter,
          path,
          ~s|type="module"|,
          ~s|type="text/javascript"|,
          ~s|type="module"|
        )

      [] ->
        Igniter.add_warning(
          igniter,
          ~s|Could not find root.html.heex. Change app.js script tag to type="module" manually.|
        )
    end
  end

  # --- app.js ---

  @llv_js """
  import { LLVEngine } from "../vendor/local_live_view.js";
  await LLVEngine.create(liveSocket, { Socket, bundlePaths: ["/assets/js/wasm/bundle.avm"] });
  """

  defp inject_app_js(igniter) do
    case Path.wildcard("assets/js/app.js") do
      [path | _] ->
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

      [] ->
        Igniter.add_warning(igniter, """
        Could not find assets/js/app.js. Add after liveSocket.connect():
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
    if File.exists?(path) do
      replace_in_file(igniter, path, "--format=esm", "--bundle", "--bundle --format=esm")
    else
      igniter
    end
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
      {:code, Sourceror.parse_string!("{LocalLiveView.Watcher, :start_link, []}")},
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

  defp generate_local_project(igniter, llv_path) do
    if File.exists?("local") do
      igniter
    else
      igniter
      |> copy_template("local/mix.exs", "mix.exs", llv_path: llv_path)
      |> copy_template("local/config/config.exs", "config.exs")
      |> copy_template("local/.formatter.exs", "formatter.exs")
      |> copy_template("local/lib/local/application.ex", "application.ex")
      |> copy_template("local/lib/hello_local.ex", "hello_local.ex")
    end
  end

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
end
