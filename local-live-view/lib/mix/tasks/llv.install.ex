defmodule Mix.Tasks.Llv.Install do
  use Mix.Task

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

  @popcorn_js_version "^0.3.0-rc1"

  @impl Mix.Task
  def run(_args) do
    app = Mix.Project.config()[:app]
    app_name = to_string(app)
    llv_path = llv_path_from_local()

    Mix.shell().info("Installing LocalLiveView into #{app_name}...")

    inject_endpoint()
    inject_application(app_name)
    inject_web_module()
    inject_root_layout()
    inject_app_js()
    inject_package_json()
    inject_mix_esbuild(app_name)
    inject_dev_watcher()
    generate_build_mjs()
    generate_vendor()
    generate_local_project(llv_path)

    print_success()
  end

  # --- Endpoint ---

  defp inject_endpoint do
    with {:ok, path} <- find_file("lib/*_web/endpoint.ex"),
         content = File.read!(path),
         changed = inject_socket(content),
         changed = inject_wasm_headers(changed) do
      if changed != content do
        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      else
        Mix.shell().info("* skipping #{path} (already configured)")
      end
    else
      :not_found ->
        warn_manual("endpoint.ex", "lib/<app>_web/endpoint.ex", """
            socket "/llv_socket", LocalLiveView.Socket, websocket: true

          And add a plug + private function for COOP/COEP headers:

            plug :put_wasm_security_headers

            defp put_wasm_security_headers(conn, _opts) do
              conn
              |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
              |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
            end
        """)
    end
  end

  defp inject_socket(content) do
    if String.contains?(content, "LocalLiveView.Socket") do
      content
    else
      inject_before(
        content,
        ~r/\n  plug /,
        "\n  socket \"/llv_socket\", LocalLiveView.Socket, websocket: true\n"
      )
    end
  end

  defp inject_wasm_headers(content) do
    if String.contains?(content, "put_wasm_security_headers") do
      content
    else
      content
      |> inject_after(~r/use Phoenix\.Endpoint[^\n]*\n/, "  plug :put_wasm_security_headers\n")
      |> inject_before_module_end("""
        defp put_wasm_security_headers(conn, _opts) do
          conn
          |> Plug.Conn.put_resp_header("cross-origin-opener-policy", "same-origin")
          |> Plug.Conn.put_resp_header("cross-origin-embedder-policy", "require-corp")
        end
      """)
    end
  end

  # --- Application ---

  defp inject_application(app_name) do
    with {:ok, path} <- find_file("lib/#{app_name}/application.ex"),
         content = File.read!(path) do
      if String.contains?(content, "LocalLiveView.ChannelRegistry") do
        Mix.shell().info("* skipping #{path} (already configured)")
      else
        changed =
          inject_after(
            content,
            ~r/children = \[\n/,
            "      {Registry, keys: :unique, name: LocalLiveView.ChannelRegistry},\n"
          )

        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      :not_found ->
        warn_manual("application.ex", "lib/#{app_name}/application.ex", """
            Add to your supervisor children:
              {Registry, keys: :unique, name: LocalLiveView.ChannelRegistry}
        """)
    end
  end

  # --- Web module (html_helpers) ---

  defp inject_web_module do
    with {:ok, path} <- find_file("lib/*_web.ex"),
         content = File.read!(path) do
      if String.contains?(content, "LocalLiveView.Component") do
        Mix.shell().info("* skipping #{path} (already configured)")
      else
        changed =
          inject_after(
            content,
            ~r/import \w+Web\.CoreComponents[^\n]*\n/,
            "      import LocalLiveView.Component\n"
          )

        changed =
          if changed == content do
            inject_after(
              content,
              ~r/import Phoenix\.HTML[^\n]*\n/,
              "      import LocalLiveView.Component\n"
            )
          else
            changed
          end

        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      :not_found ->
        warn_manual("*_web.ex", "lib/<app>_web.ex", """
            Add to your html_helpers:
              import LocalLiveView.Component
        """)
    end
  end

  # --- Root layout ---

  defp inject_root_layout do
    with {:ok, path} <- find_file("lib/*_web/components/layouts/root.html.heex"),
         content = File.read!(path) do
      if String.contains?(content, ~s|type="module"|) do
        Mix.shell().info("* skipping #{path} (already configured)")
      else
        changed = String.replace(content, ~s|type="text/javascript"|, ~s|type="module"|)
        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      :not_found ->
        warn_manual("root.html.heex", "lib/<app>_web/components/layouts/root.html.heex", """
            Change your app.js script tag to:
              <script defer phx-track-static type="module" src={~p"/assets/js/app.js"}>
        """)
    end
  end

  # --- app.js ---

  defp inject_app_js do
    with {:ok, path} <- find_file("assets/js/app.js"),
         content = File.read!(path) do
      if String.contains?(content, "local_live_view") do
        Mix.shell().info("* skipping #{path} (already configured)")
      else
        changed =
          inject_after(
            content,
            ~r/liveSocket\.connect\(\);?\n/,
            ~s|\nimport { setup } from "local_live_view";\nsetup(liveSocket, { Socket, bundlePaths: ["/assets/js/wasm/bundle.avm"] });\n|
          )

        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      :not_found ->
        warn_manual("app.js", "assets/js/app.js", """
            Add after liveSocket.connect():
              import { setup } from "local_live_view";
              setup(liveSocket, { Socket, bundlePaths: ["/assets/js/wasm/bundle.avm"] });
        """)
    end
  end

  # --- package.json ---

  defp inject_package_json do
    path = "assets/package.json"

    if File.exists?(path) do
      content = File.read!(path)

      if String.contains?(content, "@swmansion/popcorn") do
        Mix.shell().info("* skipping #{path} (already configured)")
      else
        changed =
          content
          |> inject_after(~r/"devDependencies": \{\n/, ~s|    "@swmansion/popcorn": "#{@popcorn_js_version}",\n|)
          |> inject_after(~r/"devDependencies": \{\n/, ~s|    "esbuild": "^0.25.0",\n|)

        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      copy_template("package.json", path, popcorn_version: @popcorn_js_version)
    end
  end

  # --- mix.exs esbuild replacement ---

  defp inject_mix_esbuild(app_name) do
    path = "mix.exs"

    if File.exists?(path) do
      content = File.read!(path)

      if String.contains?(content, "build_js") and String.contains?(content, ~s|"llv.build"|) do
        Mix.shell().info("* skipping #{path} esbuild (already configured)")
      else
        {ecto_in_setup, ecto_aliases} =
          if String.contains?(content, "ecto.setup") do
            {~s|"ecto.setup", |,
             ~s|      "ecto.setup": ["ecto.create", "ecto.migrate", "run priv/repo/seeds.exs"],\n| <>
               ~s|      "ecto.reset": ["ecto.drop", "ecto.setup"],\n|}
          else
            {"", ""}
          end

        aliases_block =
          "  defp aliases do\n    [\n" <>
            "      setup: [\"deps.get\", \"llv.build\", #{ecto_in_setup}\"compile\", \"assets.setup\", \"assets.build\"],\n" <>
            ecto_aliases <>
            "      \"assets.setup\": [\"tailwind.install --if-missing\", &npm_install/1],\n" <>
            "      \"assets.build\": [&build_js/1, \"tailwind #{app_name}\"],\n" <>
            "      \"assets.deploy\": [&build_js/1, \"tailwind #{app_name} --minify\", \"phx.digest\"]\n" <>
            "    ]\n  end\n"

        changed =
          content
          |> String.replace(~r/  defp aliases do\n.*?\n  end\n/s, aliases_block)
          |> inject_before_module_end("""
            defp npm_install(_) do
              {_, 0} =
                System.cmd("npm", ["install"],
                  cd: Path.join(File.cwd!(), "assets"),
                  into: IO.stream(:stdio, :line),
                  stderr_to_stdout: true
                )
            end

            defp build_js(_) do
              {_, 0} =
                System.cmd("node", ["build.mjs"],
                  cd: Path.join(File.cwd!(), "assets"),
                  env: [{"MIX_BUILD_PATH", Mix.Project.build_path()}],
                  into: IO.stream(:stdio, :line),
                  stderr_to_stdout: true
                )
            end
          """)

        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      end
    else
      warn_manual("mix.exs", "mix.exs", """
          Replace defp aliases and add pnpm_install/build_js functions.
      """)
    end
  end

  # --- config/dev.exs watcher ---

  defp inject_dev_watcher do
    with {:ok, path} <- find_file("config/dev.exs"),
         content = File.read!(path) do
      changed =
        if String.contains?(content, "build.mjs") do
          content
        else
          String.replace(
            content,
            ~r/esbuild: \{Esbuild,[^\}]+\},?\n/,
            "node: [\n      \"build.mjs\",\n      \"--watch\",\n      cd: Path.expand(\"../assets\", __DIR__),\n      env: %{\"MIX_BUILD_PATH\" => Mix.Project.build_path()}\n    ],\n"
          )
        end

      changed =
        if String.contains?(changed, "web_console_logger: false") do
          changed
        else
          String.replace(changed, "web_console_logger: true", "web_console_logger: false")
        end

      if changed != content do
        File.write!(path, changed)
        Mix.shell().info("* injecting #{path}")
      else
        Mix.shell().info("* skipping #{path} (already configured)")
      end
    else
      :not_found ->
        warn_manual("dev.exs", "config/dev.exs", """
            Replace esbuild watcher with:
              node: ["build.mjs", "--watch",
                cd: Path.expand("../assets", __DIR__),
                env: %{"MIX_BUILD_PATH" => Mix.Project.build_path()}]

            And set in live_reload config:
              web_console_logger: false
        """)
    end
  end

  # --- assets/build.mjs ---

  defp generate_build_mjs do
    path = "assets/build.mjs"

    if File.exists?(path) do
      Mix.shell().info("* skipping #{path} (already exists)")
    else
      copy_template("build.mjs", path)
    end
  end

  # --- assets/vendor/local_live_view.js ---

  defp generate_vendor do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    src = Path.join(llv_abs, "assets/local_live_view.js")
    dest = "assets/vendor/local_live_view.js"

    File.mkdir_p!("assets/vendor")
    File.copy!(src, dest)
    Mix.shell().info("* vendoring #{dest}")
  end

  # --- Generate local/ ---

  defp generate_local_project(llv_path) do
    if File.exists?("local") do
      Mix.shell().info("* skipping local/ (already exists)")
    else
      Mix.shell().info("* generating local/")
      File.mkdir_p!("local/config")
      File.mkdir_p!("local/lib/local")

      copy_template("mix.exs", "local/mix.exs", llv_path: llv_path)
      copy_template("config.exs", "local/config/config.exs")
      copy_template("formatter.exs", "local/.formatter.exs")
      copy_template("application.ex", "local/lib/local/application.ex")
      copy_template("hello_local.ex", "local/lib/hello_local.ex")
    end
  end

  defp llv_path_from_local do
    llv_abs = Mix.Project.deps_paths()[:local_live_view]
    local_abs = Path.join(File.cwd!(), "local")
    Path.relative_to(llv_abs, local_abs, force: true)
  end

  # --- Helpers ---

  defp find_file(pattern) do
    case Path.wildcard(pattern) do
      [path | _] -> {:ok, path}
      [] -> :not_found
    end
  end

  defp inject_after(content, pattern, injection) do
    case Regex.run(pattern, content, return: :index) do
      [{start, len}] ->
        {before, rest} = String.split_at(content, start + len)
        before <> injection <> rest

      _ ->
        content
    end
  end

  defp inject_before(content, pattern, injection) do
    case Regex.run(pattern, content, return: :index) do
      [{start, _len}] ->
        {before, rest} = String.split_at(content, start)
        before <> injection <> rest

      _ ->
        content
    end
  end

  defp inject_before_module_end(content, injection) do
    [body | _] = Regex.split(~r/\nend\s*\z/, content)
    body <> "\n\n" <> injection <> "end\n"
  end

  @templates_dir Path.expand("../../../priv/templates/llv.install", __DIR__)

  defp copy_template(template, dest, bindings \\ []) do
    content = File.read!(Path.join(@templates_dir, template))

    content =
      if bindings == [] do
        content
      else
        EEx.eval_string(content, assigns: bindings)
      end

    File.write!(dest, content)
    Mix.shell().info("* creating #{dest}")
  end

  defp warn_manual(name, expected_path, instructions) do
    Mix.shell().error("""
    Could not find #{name} at #{expected_path}. Add manually:
    #{instructions}
    """)
  end

  defp print_success do
    Mix.shell().info("""

    LocalLiveView installed successfully!

    Next steps:

      1. Run setup (installs JS deps, builds WASM bundle, compiles):
           mix setup

      2. Start the server:
           mix phx.server

      3. Mount the sample view in any LiveView template:
           <.local_live_view view="HelloLocal" />

    See local/lib/hello_local.ex for the generated example.
    """)
  end
end
