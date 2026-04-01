defmodule Mix.Tasks.Llv.Install do
  use Mix.Task

  @shortdoc "Installs LocalLiveView into a Phoenix project"

  @moduledoc """
  Installs LocalLiveView into an existing Phoenix project.

      $ mix llv.install

  Must be run from the root of a Phoenix project generated with `mix phx.new`.

  ## Options

    * `--dry-run` - print all changes without writing any files

  ## What it does

    * Creates a `local/` sub-project with a starter LocalLiveView
    * Modifies `router.ex` to add required CORS/COEP headers
    * Modifies `{app}_web.ex` to add static path and helper import
    * Modifies `components/core_components.ex` to add `<.local_live_view>` component
    * Modifies `assets/js/app.js` to add the hook and setup call
    * Replaces `assets/build.mjs` with a Popcorn-compatible esbuild config
    * Modifies `assets/package.json` to add `local_live_view` dependency
    * Modifies `mix.exs` to add build aliases and remove esbuild
    * Modifies `config/config.exs` to remove esbuild config
    * Modifies `config/dev.exs` to replace esbuild watcher with node
    * Creates `pnpm-workspace.yaml` if it does not exist
    * Creates `lib/{app}_web/local_live_view.ex` helper

  """

  @impl Mix.Task
  def run(args) do
    {opts, _} = OptionParser.parse!(args, strict: [dry_run: :boolean])
    dry_run? = Keyword.get(opts, :dry_run, false)

    app = Mix.Project.config()[:app] |> to_string()
    app_module = Macro.camelize(app)
    web_module = app_module <> "Web"
    llv_version = llv_minor_version()
    llv_source = llv_source_path()
    llv_path_from_local = llv_source |> Path.expand() |> Path.relative_to(Path.expand("local"))
    llv_assets_path = Path.join(llv_source, "assets") |> Path.expand() |> Path.relative_to(File.cwd!())

    assigns = [app: app, module: app_module, web_module: web_module, llv_version: llv_version, llv_path: llv_path_from_local]

    if dry_run? do
      Mix.shell().info([:cyan, "Running in --dry-run mode. No files will be written.\n", :reset])
    end

    create_local_project(assigns, dry_run?)
    create_llv_helper(app, web_module, dry_run?)
    modify_mix_exs(app, dry_run?)
    modify_config_exs(dry_run?)
    modify_dev_exs(dry_run?)
    modify_endpoint(dry_run?)
    modify_router(dry_run?)
    modify_web_module(app, web_module, dry_run?)
    modify_core_components(dry_run?)
    modify_app_js(dry_run?)
    modify_root_layout(dry_run?)
    replace_build_mjs(dry_run?)
    modify_package_json(dry_run?)
    create_pnpm_workspace(llv_assets_path, dry_run?)

    print_instructions(app, app_module)
  end

  # ---------------------------------------------------------------------------
  # Create local/ project from templates
  # ---------------------------------------------------------------------------

  defp create_local_project(assigns, dry_run?) do
    t = &template_path/1

    copy_template(t.("local/mix.exs.eex"), "local/mix.exs", assigns, dry_run?)
    copy_file(t.("local/config/config.exs"), "local/config/config.exs", dry_run?)
    copy_file(t.("local/.formatter.exs"), "local/.formatter.exs", dry_run?)
    copy_file(t.("local/.gitignore"), "local/.gitignore", dry_run?)
    copy_file(t.("local/lib/application.ex"), "local/lib/local/application.ex", dry_run?)
    copy_template(t.("local/lib/app_live.ex.eex"), "local/lib/#{assigns[:app]}_live.ex", assigns, dry_run?)
    copy_file(t.("local/test/test_helper.exs"), "local/test/test_helper.exs", dry_run?)
  end

  # ---------------------------------------------------------------------------
  # Create lib/{app}_web/local_live_view.ex helper
  # ---------------------------------------------------------------------------

  defp create_llv_helper(app, web_module, dry_run?) do
    content = """
    defmodule #{web_module}.LocalLiveView do
      @moduledoc false

      import Phoenix.LiveView, only: [push_event: 3]

      def push_to_local(socket, view, payload) do
        push_event(socket, "llv_server_message", %{"view" => view, "payload" => payload})
      end
    end
    """

    write_file("lib/#{app}_web/local_live_view.ex", content, dry_run?)
  end

  # ---------------------------------------------------------------------------
  # mix.exs
  # ---------------------------------------------------------------------------

  defp modify_mix_exs(_app, dry_run?) do
    path = "mix.exs"
    content = File.read!(path)

    content = Regex.replace(~r/,?\s*\{:esbuild,[^}]+\}/, content, "")
    content = Regex.replace(~r/,?\s*"esbuild[^"]*"/, content, "")
    content = Regex.replace(~r/"assets\.build":\s*\[/, content, "\"assets.build\": [&build_js/1, ")
    content = Regex.replace(~r/"assets\.deploy":\s*\[/, content, "\"assets.deploy\": [&build_js/1, ")
    content = fix_setup_alias(content)
    content = inject_before_last_end(content, mix_helper_functions())

    modify_file(path, content, dry_run?)
  end

  defp fix_setup_alias(content) do
    content = Regex.replace(~r/&build_local\/1,\s*&pnpm_install\/1,\s*/, content, "")

    Regex.replace(
      ~r/(setup:\s*\["deps\.get",)/,
      content,
      fn _, prefix -> prefix <> " &build_local/1, &pnpm_install/1," end
    )
  end

  defp mix_helper_functions do
    """

  defp build_local(_) do
    Mix.shell().cmd("mix build", cd: "local")
  end

  defp pnpm_install(_) do
    {_, 0} =
      System.cmd("pnpm", ["install"],
        cd: File.cwd!(),
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
  end

  defp build_js(_) do
    {_, 0} =
      System.cmd("pnpm", ["run", "build"],
        cd: Path.join(File.cwd!(), "assets"),
        env: [{"MIX_BUILD_PATH", Mix.Project.build_path()}],
        into: IO.stream(:stdio, :line),
        stderr_to_stdout: true
      )
  end

"""
  end

  # ---------------------------------------------------------------------------
  # config/config.exs — remove esbuild block
  # ---------------------------------------------------------------------------

  defp modify_config_exs(dry_run?) do
    path = "config/config.exs"
    content = File.read!(path)

    content =
      Regex.replace(
        ~r/\n# Configure esbuild[^\n]*\nconfig :esbuild,\n(?:[ \t][^\n]*\n)*/,
        content,
        "\n"
      )

    modify_file(path, content, dry_run?)
  end

  # ---------------------------------------------------------------------------
  # config/dev.exs — replace esbuild watcher with node
  # ---------------------------------------------------------------------------

  defp modify_dev_exs(dry_run?) do
    path = "config/dev.exs"
    content = File.read!(path)

    content =
      Regex.replace(
        ~r/esbuild: \{Esbuild, :install_and_run, \[.*?\]\},?/s,
        content,
        node_watcher()
      )

    modify_file(path, content, dry_run?)
  end

  defp node_watcher do
    """
    node: [
          "build.mjs",
          "--watch",
          cd: Path.expand("../assets", __DIR__),
          env: %{"MIX_BUILD_PATH" => Mix.Project.build_path()}
        ],
    """
  end

  # ---------------------------------------------------------------------------
  # endpoint.ex — add CORS/COEP headers to Plug.Static
  # ---------------------------------------------------------------------------

  defp modify_endpoint(dry_run?) do
    path = endpoint_path()
    content = File.read!(path)

    content =
      cond do
        String.contains?(content, "raise_on_missing_only: code_reloading?") ->
          String.replace(
            content,
            "    raise_on_missing_only: code_reloading?",
            "    raise_on_missing_only: code_reloading?,\n" <> static_headers_option()
          )

        String.contains?(content, "plug Plug.Static,") ->
          Regex.replace(
            ~r/(gzip: [^\n]+)/,
            content,
            "\\1,\n" <> static_headers_option()
          )

        true ->
          content
      end

    modify_file(path, content, dry_run?)
  end

  defp static_headers_option do
    ~s(    headers: %{\n) <>
      ~s(      "access-control-allow-origin" => "*",\n) <>
      ~s(      "cross-origin-opener-policy" => "same-origin",\n) <>
      ~s(      "cross-origin-embedder-policy" => "require-corp",\n) <>
      ~s(      "cache-control" => "public no-cache"\n) <>
      ~s(    })
  end

  defp endpoint_path do
    case Path.wildcard("lib/*_web/endpoint.ex") do
      [path | _] -> path
      [] -> raise "Could not find endpoint.ex. Are you in a Phoenix project root?"
    end
  end

  # ---------------------------------------------------------------------------
  # router.ex — add CORS/COEP headers
  # ---------------------------------------------------------------------------

  defp modify_router(dry_run?) do
    path = router_path()
    content = File.read!(path)

    content =
      content
      |> String.replace(
        "plug :put_secure_browser_headers\n  end",
        "plug :put_secure_browser_headers\n    plug :add_headers\n  end"
      )
      |> String.replace(
        "plug :accepts, [\"json\"]\n  end",
        "plug :accepts, [\"json\"]\n    plug :add_headers\n  end"
      )
      |> inject_before_last_end(add_headers_function())

    modify_file(path, content, dry_run?)
  end

  defp add_headers_function do
    """

  def add_headers(conn, _opts) do
    Plug.Conn.merge_resp_headers(conn, [
      {"Access-Control-Allow-Origin", "*"},
      {"Cross-Origin-Opener-Policy", "same-origin"},
      {"Cross-Origin-Embedder-Policy", "require-corp"},
      {"Cache-Control", "public no-cache"}
    ])
  end
"""
  end

  # ---------------------------------------------------------------------------
  # {app}_web.ex — static_paths + import in live_view
  # ---------------------------------------------------------------------------

  defp modify_web_module(app, web_module, dry_run?) do
    path = "lib/#{app}_web.ex"
    content = File.read!(path)

    content =
      content
      |> String.replace(
        "~w(assets fonts images favicon.ico robots.txt)",
        "~w(assets fonts images favicon.ico robots.txt local_live_view)"
      )
      |> String.replace(
        "use Phoenix.LiveView\n\n      unquote(html_helpers())",
        "use Phoenix.LiveView\n\n      import #{web_module}.LocalLiveView\n\n      unquote(html_helpers())"
      )

    modify_file(path, content, dry_run?)
  end

  # ---------------------------------------------------------------------------
  # core_components.ex — add <.local_live_view> component
  # ---------------------------------------------------------------------------

  defp modify_core_components(dry_run?) do
    path = core_components_path()
    content = File.read!(path)

    content = inject_before_last_end(content, local_live_view_component())

    modify_file(path, content, dry_run?)
  end

  defp local_live_view_component do
    File.read!(template_path("core_components_component.ex"))
  end

  # ---------------------------------------------------------------------------
  # assets/js/app.js — add hook + setup
  # ---------------------------------------------------------------------------

  defp modify_app_js(dry_run?) do
    path = "assets/js/app.js"
    content = File.read!(path)

    content =
      cond do
        # Phoenix <= 1.7: has const Hooks = {}
        String.contains?(content, "const Hooks = {};") ->
          content
          |> String.replace(
            "const Hooks = {};",
            "const Hooks = {};\n\n" <> hook_property_assignment()
          )
          |> String.replace("liveSocket.connect();", "liveSocket.connect();\n\n" <> llv_setup())

        # Phoenix 1.8+: no Hooks object, uses spread in LiveSocket constructor
        String.contains?(content, "new LiveSocket(") ->
          content1 =
            String.replace(
              content,
              "const liveSocket = new LiveSocket(",
              hook_const_declaration() <> "\nconst liveSocket = new LiveSocket("
            )

          content2 = Regex.replace(~r/hooks: \{([^}]*)\}/, content1, "hooks: {\\1, LocalLiveViewHook}")

          String.replace(content2, "liveSocket.connect();", "liveSocket.connect();\n\n" <> llv_setup())

        true ->
          content
      end

    modify_file(path, content, dry_run?)
  end

  defp hook_body do
    """
{
  mounted() {
    this.el.addEventListener("serverSend", (e) => {
      this.pushEvent(e.detail.event_name, e.detail.payload, (reply) => {
        console.debug(reply.message);
      });
    });
  },
  updated() {
    this.el.setAttribute("data-phx-root-id", this.el.id);
    this.el.setAttribute("data-phx-session", this.el.id);

    const newAttrsStr = this.el.getAttribute("data-pop-attrs") || "{}";
    if (newAttrsStr === this._prevAttrsStr) return;
    this._prevAttrsStr = newAttrsStr;
    const newAttrs = JSON.parse(newAttrsStr);
    window.__popcorn
      ?.call(
        { id: this.el.id, event: "llv_attrs_update", payload: newAttrs },
        { timeoutMs: 10_000 },
      )
      .catch((err) => console.error("LLV attrs update error", err));
  },
}
"""
  end

  defp hook_property_assignment do
    "Hooks.LocalLiveViewHook = " <> String.trim_trailing(hook_body()) <> ";\n"
  end

  defp hook_const_declaration do
    "const LocalLiveViewHook = " <> String.trim_trailing(hook_body()) <> ";\n"
  end

  defp llv_setup do
    """
import { setup } from "local_live_view";
setup(liveSocket, { bundlePath: "bundle.avm" });
"""
  end

  # ---------------------------------------------------------------------------
  # layouts/root.html.heex — change script type to module
  # ---------------------------------------------------------------------------

  defp modify_root_layout(dry_run?) do
    case Path.wildcard("lib/*_web/components/layouts/root.html.heex") do
      [path | _] ->
        content =
          path
          |> File.read!()
          |> String.replace(~s(type="text/javascript"), ~s(type="module"))

        modify_file(path, content, dry_run?)

      [] ->
        Mix.shell().info([:yellow, "* skipping root.html.heex (not found)", :reset])
    end
  end

  # ---------------------------------------------------------------------------
  # assets/build.mjs — replace with Popcorn-compatible build
  # ---------------------------------------------------------------------------

  defp replace_build_mjs(dry_run?) do
    content = File.read!(template_path("assets/build.mjs"))
    write_file("assets/build.mjs", content, dry_run?)
  end

  # ---------------------------------------------------------------------------
  # assets/package.json — add local_live_view workspace dep
  # ---------------------------------------------------------------------------

  defp modify_package_json(dry_run?) do
    path = "assets/package.json"

    content =
      if File.exists?(path) do
        path
        |> File.read!()
        |> String.replace(
          ~s("dependencies": {),
          ~s("dependencies": {\n    "local_live_view": "workspace:*",)
        )
      else
        """
        {
          "name": "app",
          "private": true,
          "dependencies": {
            "local_live_view": "workspace:*"
          },
          "scripts": {
            "build": "node build.mjs",
            "watch": "node build.mjs --watch"
          },
          "devDependencies": {
            "esbuild": "^0.25.0"
          }
        }
        """
      end

    modify_file(path, content, dry_run?)
  end

  # ---------------------------------------------------------------------------
  # pnpm-workspace.yaml
  # ---------------------------------------------------------------------------

  defp create_pnpm_workspace(llv_assets_path, dry_run?) do
    path = "pnpm-workspace.yaml"

    unless File.exists?(path) or parent_workspace_exists?() do
      content = """
      packages:
        - "assets"
        - "#{llv_assets_path}"
      """

      write_file(path, content, dry_run?)
    end
  end

  defp parent_workspace_exists? do
    File.cwd!()
    |> Path.dirname()
    |> Stream.iterate(&Path.dirname/1)
    |> Stream.take(5)
    |> Enum.any?(fn dir -> File.exists?(Path.join(dir, "pnpm-workspace.yaml")) end)
  end

  # ---------------------------------------------------------------------------
  # File helpers
  # ---------------------------------------------------------------------------

  defp copy_template(template_path, dest, assigns, dry_run?) do
    content = EEx.eval_file(template_path, assigns: assigns)
    write_file(dest, content, dry_run?)
  end

  defp copy_file(template_path, dest, dry_run?) do
    content = File.read!(template_path)
    write_file(dest, content, dry_run?)
  end

  defp write_file(path, _content, true = _dry_run?) do
    Mix.shell().info([:yellow, "* would create ", :reset, path])
  end

  defp write_file(path, content, false = _dry_run?) do
    File.mkdir_p!(Path.dirname(path))
    File.write!(path, content)
    Mix.shell().info([:green, "* created ", :reset, path])
  end

  defp modify_file(path, _content, true = _dry_run?) do
    Mix.shell().info([:yellow, "* would modify ", :reset, path])
  end

  defp modify_file(path, content, false = _dry_run?) do
    File.write!(path, content)
    Mix.shell().info([:green, "* modified ", :reset, path])
  end

  # ---------------------------------------------------------------------------
  # String manipulation helpers
  # ---------------------------------------------------------------------------

  defp inject_before_last_end(content, injection) do
    Regex.replace(~r/\nend\s*\z/, content, injection <> "\nend\n")
  end

# ---------------------------------------------------------------------------
  # Path helpers
  # ---------------------------------------------------------------------------

  defp template_path(subpath) do
    Application.app_dir(:local_live_view, "priv/templates/llv.install/#{subpath}")
  end

  defp router_path do
    case Path.wildcard("lib/*_web/router.ex") do
      [path | _] -> path
      [] -> raise "Could not find router.ex. Are you in a Phoenix project root?"
    end
  end

  defp core_components_path do
    case Path.wildcard("lib/*_web/components/core_components.ex") do
      [path | _] -> path
      [] -> raise "Could not find core_components.ex. Are you in a Phoenix project root?"
    end
  end

  defp llv_minor_version do
    case Application.spec(:local_live_view, :vsn) do
      nil ->
        "0.1"

      vsn ->
        vsn
        |> to_string()
        |> String.split(".")
        |> Enum.take(2)
        |> Enum.join(".")
    end
  end

  defp llv_source_path do
    deps = Mix.Project.config()[:deps] || []

    case Enum.find(deps, fn dep -> elem(dep, 0) == :local_live_view end) do
      {_, opts} when is_list(opts) ->
        Keyword.get(opts, :path, "deps/local_live_view")

      {_, _version, opts} when is_list(opts) ->
        Keyword.get(opts, :path, "deps/local_live_view")

      _ ->
        "deps/local_live_view"
    end
  end

  # ---------------------------------------------------------------------------
  # Post-install instructions
  # ---------------------------------------------------------------------------

  defp print_instructions(app, app_module) do
    Mix.shell().info("""

    \e[32mLocalLiveView installed successfully!\e[0m

    Next steps:

      1. Install pnpm if you haven't already:
           npm install -g pnpm

      2. Build everything:
           mix setup

      3. Start the dev server:
           mix phx.server

    Your starter LocalLiveView is in \e[36mlocal/lib/#{app}_live.ex\e[0m

    Mount it in a Phoenix LiveView template with:

        <.local_live_view view="#{app_module}Live" id="#{app_module}Live" />

    """)
  end
end
